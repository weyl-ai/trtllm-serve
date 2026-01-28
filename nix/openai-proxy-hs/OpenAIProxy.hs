{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : OpenAIProxy
Description : Unified AI Gateway - OpenAI proxy + tools + metrics
License     : MIT

Single Haskell server providing:
- OpenAI-compatible chat completions (streaming, non-streaming)
- Tool endpoints (search, URL reader)
- Metrics to ClickHouse (tokens, bytes, latency)
-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (SomeException, catch)
import Control.Monad (unless, when, void, forever)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (fromText)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as Builder
import Data.IORef
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import GHC.Generics
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as TLS
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import Text.Read (readMaybe)

import qualified ChatTemplate as CT


-- ════════════════════════════════════════════════════════════════════════════════
-- Configuration
-- ════════════════════════════════════════════════════════════════════════════════

data Config = Config
  { cfgPort         :: !Int
  , cfgTritonUrl    :: !String     -- e.g., "http://localhost:8000"
  , cfgModelName    :: !Text
  , cfgMaxRetries   :: !Int
  , cfgTimeoutMs    :: !Int
  -- Tool backends
  , cfgSearxngUrl   :: !(Maybe Text)
  , cfgExaApiKey    :: !(Maybe Text)
  , cfgJinaApiKey   :: !(Maybe Text)
  -- Metrics
  , cfgMetricsEnabled    :: !Bool
  , cfgClickhouseUrl     :: !Text
  , cfgClickhouseDb      :: !Text
  -- Think block handling
  , cfgStripThinking     :: !Bool   -- Strip <think>...</think> blocks from output
  } deriving (Show)

defaultConfig :: Config
defaultConfig = Config
  { cfgPort       = 9000
  , cfgTritonUrl  = "http://localhost:8000"
  , cfgModelName  = "qwen3"
  , cfgMaxRetries = 3
  , cfgTimeoutMs  = 180000
  , cfgSearxngUrl = Nothing
  , cfgExaApiKey  = Nothing
  , cfgJinaApiKey = Nothing
  , cfgMetricsEnabled = False
  , cfgClickhouseUrl = "http://localhost:8123"
  , cfgClickhouseDb = "ai_services"
  , cfgStripThinking = True  -- Default: strip thinking for cleaner output
  }

loadConfig :: IO Config
loadConfig = do
  port <- maybe 9000 id . (>>= readMaybe) <$> lookupEnv "OPENAI_PROXY_PORT"
  tritonUrl <- fromMaybe "http://localhost:8000" <$> lookupEnv "TRITON_URL"
  modelName <- maybe "qwen3" T.pack <$> lookupEnv "MODEL_NAME"
  searxng <- fmap T.pack <$> lookupEnv "SEARXNG_URL"
  exa <- fmap T.pack <$> lookupEnv "EXA_API_KEY"
  jina <- fmap T.pack <$> lookupEnv "JINA_API_KEY"
  metricsEnabled <- maybe False (== "1") <$> lookupEnv "METRICS_ENABLED"
  clickhouseUrl <- maybe "http://localhost:8123" T.pack <$> lookupEnv "CLICKHOUSE_URL"
  clickhouseDb <- maybe "ai_services" T.pack <$> lookupEnv "CLICKHOUSE_DATABASE"
  -- STRIP_THINKING: "0" or "false" to show thinking, default is to strip
  stripThinking <- maybe True (\v -> v /= "0" && v /= "false") <$> lookupEnv "STRIP_THINKING"
  pure $ defaultConfig
    { cfgPort = port
    , cfgTritonUrl = tritonUrl
    , cfgModelName = modelName
    , cfgSearxngUrl = searxng
    , cfgExaApiKey = exa
    , cfgJinaApiKey = jina
    , cfgMetricsEnabled = metricsEnabled
    , cfgClickhouseUrl = clickhouseUrl
    , cfgClickhouseDb = clickhouseDb
    , cfgStripThinking = stripThinking
    }


-- ════════════════════════════════════════════════════════════════════════════════
-- Metrics
-- ════════════════════════════════════════════════════════════════════════════════

data RequestMetrics = RequestMetrics
  { rmTimestamp    :: !UTCTime
  , rmRequestId    :: !Text
  , rmEndpoint     :: !Text
  , rmModel        :: !Text
  , rmTokensIn     :: !Int       -- Input tokens (estimated from prompt)
  , rmTokensOut    :: !Int       -- Output tokens (counted from response)
  , rmBytesIn      :: !Int
  , rmBytesOut     :: !Int
  , rmLatencyMs    :: !Double
  , rmTtftMs       :: !(Maybe Double)  -- Time to first token (streaming)
  , rmStatusCode   :: !Int
  , rmStream       :: !Bool
  , rmClientIP     :: !Text
  } deriving (Show)

instance ToJSON RequestMetrics where
  toJSON RequestMetrics{..} = object $ catMaybes
    [ Just $ "timestamp"   .= iso8601Show rmTimestamp
    , Just $ "request_id"  .= rmRequestId
    , Just $ "endpoint"    .= rmEndpoint
    , Just $ "model"       .= rmModel
    , Just $ "tokens_in"   .= rmTokensIn
    , Just $ "tokens_out"  .= rmTokensOut
    , Just $ "bytes_in"    .= rmBytesIn
    , Just $ "bytes_out"   .= rmBytesOut
    , Just $ "latency_ms"  .= rmLatencyMs
    , ("ttft_ms" .=) <$> rmTtftMs
    , Just $ "status_code" .= rmStatusCode
    , Just $ "stream"      .= rmStream
    , Just $ "client_ip"   .= rmClientIP
    ]

data MetricsState = MetricsState
  { msConfig  :: !Config
  , msBuffer  :: !(MVar [RequestMetrics])
  , msManager :: !HC.Manager
  }

newMetricsState :: Config -> HC.Manager -> IO MetricsState
newMetricsState cfg mgr = do
  buffer <- newMVar []
  let state = MetricsState cfg buffer mgr
  when (cfgMetricsEnabled cfg) $ void $ forkIO $ flushThread state
  pure state

flushThread :: MetricsState -> IO ()
flushThread state = forever $ do
  threadDelay 10000000  -- 10 seconds
  flushMetrics state `catch` \(_ :: SomeException) -> pure ()

recordMetrics :: MetricsState -> RequestMetrics -> IO ()
recordMetrics state metrics
  | not (cfgMetricsEnabled (msConfig state)) = pure ()
  | otherwise = do
      shouldFlush <- modifyMVar (msBuffer state) $ \buf ->
        let newBuf = metrics : buf
        in pure (if length newBuf >= 100 then [] else newBuf, length newBuf >= 100)
      when shouldFlush $ flushMetrics state

flushMetrics :: MetricsState -> IO ()
flushMetrics state
  | not (cfgMetricsEnabled (msConfig state)) = pure ()
  | otherwise = do
      metrics <- modifyMVar (msBuffer state) $ \buf -> pure ([], buf)
      when (not (null metrics)) $ do
        let cfg = msConfig state
            rows = LBS.intercalate "\n" $ map encode metrics
            url = T.unpack (cfgClickhouseUrl cfg)
                  <> "/?database=" <> T.unpack (cfgClickhouseDb cfg)
                  <> "&query=INSERT%20INTO%20proxy_metrics%20FORMAT%20JSONEachRow"
        initReq <- HC.parseRequest url
        let req = initReq
              { HC.method = "POST"
              , HC.requestBody = HC.RequestBodyLBS rows
              , HC.requestHeaders = [("Content-Type", "application/json")]
              }
        void (HC.httpLbs req (msManager state)) `catch` \(_ :: SomeException) -> pure ()

-- Estimate tokens from text (rough: ~4 chars per token for English)
estimateTokens :: Text -> Int
estimateTokens t = max 1 $ T.length t `div` 4


-- ════════════════════════════════════════════════════════════════════════════════
-- OpenAI API Types
-- ════════════════════════════════════════════════════════════════════════════════

data ChatMessage = ChatMessage
  { msgRole    :: !Text
  , msgContent :: !(Maybe Text)
  } deriving (Show, Generic)

instance FromJSON ChatMessage where
  parseJSON = withObject "ChatMessage" $ \v -> ChatMessage
    <$> v .: "role"
    <*> v .:? "content"

instance ToJSON ChatMessage where
  toJSON ChatMessage{..} = object
    [ "role" .= msgRole
    , "content" .= msgContent
    ]

data ChatRequest = ChatRequest
  { reqModel       :: !Text
  , reqMessages    :: ![ChatMessage]
  , reqTemperature :: !(Maybe Double)
  , reqTopP        :: !(Maybe Double)
  , reqMaxTokens   :: !(Maybe Int)
  , reqStream      :: !(Maybe Bool)
  } deriving (Show, Generic)

instance FromJSON ChatRequest where
  parseJSON = withObject "ChatRequest" $ \v -> ChatRequest
    <$> v .: "model"
    <*> v .: "messages"
    <*> v .:? "temperature"
    <*> v .:? "top_p"
    <*> v .:? "max_tokens"
    <*> v .:? "stream"

data ChatChoice = ChatChoice
  { choiceIndex        :: !Int
  , choiceMessage      :: !ChatMessage
  , choiceFinishReason :: !Text
  } deriving (Show, Generic)

instance ToJSON ChatChoice where
  toJSON ChatChoice{..} = object
    [ "index" .= choiceIndex
    , "message" .= choiceMessage
    , "finish_reason" .= choiceFinishReason
    ]

data UsageInfo = UsageInfo
  { usagePromptTokens     :: !Int
  , usageCompletionTokens :: !Int
  , usageTotalTokens      :: !Int
  } deriving (Show, Generic)

instance ToJSON UsageInfo where
  toJSON UsageInfo{..} = object
    [ "prompt_tokens"     .= usagePromptTokens
    , "completion_tokens" .= usageCompletionTokens
    , "total_tokens"      .= usageTotalTokens
    ]

data ChatResponse = ChatResponse
  { respId      :: !Text
  , respObject  :: !Text
  , respCreated :: !Int
  , respModel   :: !Text
  , respChoices :: ![ChatChoice]
  , respUsage   :: !UsageInfo
  } deriving (Show, Generic)

instance ToJSON ChatResponse where
  toJSON ChatResponse{..} = object
    [ "id" .= respId
    , "object" .= respObject
    , "created" .= respCreated
    , "model" .= respModel
    , "choices" .= respChoices
    , "usage" .= respUsage
    ]

-- Streaming chunk types
data ChunkDelta = ChunkDelta
  { deltaContent :: !(Maybe Text)
  , deltaRole    :: !(Maybe Text)
  } deriving (Show, Generic)

instance ToJSON ChunkDelta where
  toJSON ChunkDelta{..} = object $ catMaybes
    [ ("content" .=) <$> deltaContent
    , ("role" .=) <$> deltaRole
    ]

data StreamChoice = StreamChoice
  { schIndex        :: !Int
  , schDelta        :: !ChunkDelta
  , schFinishReason :: !(Maybe Text)
  } deriving (Show, Generic)

instance ToJSON StreamChoice where
  toJSON StreamChoice{..} = object
    [ "index" .= schIndex
    , "delta" .= schDelta
    , "finish_reason" .= schFinishReason
    ]

data StreamChunk = StreamChunk
  { scId      :: !Text
  , scObject  :: !Text
  , scCreated :: !Int
  , scModel   :: !Text
  , scChoices :: ![StreamChoice]
  } deriving (Show, Generic)

instance ToJSON StreamChunk where
  toJSON StreamChunk{..} = object
    [ "id" .= scId
    , "object" .= scObject
    , "created" .= scCreated
    , "model" .= scModel
    , "choices" .= scChoices
    ]

-- Models endpoint
data ModelInfo = ModelInfo
  { miId      :: !Text
  , miObject  :: !Text
  , miCreated :: !Int
  , miOwnedBy :: !Text
  } deriving (Show, Generic)

instance ToJSON ModelInfo where
  toJSON ModelInfo{..} = object
    [ "id" .= miId
    , "object" .= miObject
    , "created" .= miCreated
    , "owned_by" .= miOwnedBy
    ]

data ModelsResponse = ModelsResponse
  { mrObject :: !Text
  , mrData   :: ![ModelInfo]
  } deriving (Show, Generic)

instance ToJSON ModelsResponse where
  toJSON ModelsResponse{..} = object
    [ "object" .= mrObject
    , "data" .= mrData
    ]


-- ════════════════════════════════════════════════════════════════════════════════
-- Tool Types
-- ════════════════════════════════════════════════════════════════════════════════

data SearchResult = SearchResult
  { srTitle   :: !Text
  , srUrl     :: !Text
  , srContent :: !Text
  } deriving (Show, Generic)

instance ToJSON SearchResult where
  toJSON SearchResult{..} = object
    [ "title"   .= srTitle
    , "url"     .= srUrl
    , "content" .= srContent
    ]

data SearchResponse = SearchResponse
  { searchStatus  :: !Text
  , searchQuery   :: !Text
  , searchResults :: ![SearchResult]
  , searchError   :: !(Maybe Text)
  } deriving (Show, Generic)

instance ToJSON SearchResponse where
  toJSON SearchResponse{..} = object $ catMaybes
    [ Just $ "status"  .= searchStatus
    , Just $ "query"   .= searchQuery
    , Just $ "results" .= searchResults
    , ("error" .=) <$> searchError
    ]

data ReadUrlResponse = ReadUrlResponse
  { readStatus  :: !Text
  , readUrl     :: !Text
  , readTitle   :: !(Maybe Text)
  , readContent :: !Text
  , readError   :: !(Maybe Text)
  } deriving (Show, Generic)

instance ToJSON ReadUrlResponse where
  toJSON ReadUrlResponse{..} = object $ catMaybes
    [ Just $ "status"  .= readStatus
    , Just $ "url"     .= readUrl
    , ("title" .=) <$> readTitle
    , Just $ "content" .= readContent
    , ("error" .=) <$> readError
    ]


-- ════════════════════════════════════════════════════════════════════════════════
-- Chat Template
-- ════════════════════════════════════════════════════════════════════════════════

applyChatTemplate :: [ChatMessage] -> Text
applyChatTemplate msgs = CT.renderChat chat
  where
    chat = CT.fromOpenAI [(msgRole m, fromMaybe "" (msgContent m)) | m <- msgs]


-- ════════════════════════════════════════════════════════════════════════════════
-- HTTP Clients
-- ════════════════════════════════════════════════════════════════════════════════

createManager :: IO HC.Manager
createManager = HC.newManager $ TLS.tlsManagerSettings
  { HC.managerConnCount = 10
  , HC.managerIdleConnectionCount = 5
  , HC.managerResponseTimeout = HC.responseTimeoutMicro (180 * 1000000)
  }

-- Triton generate (non-streaming)
callTritonGenerate :: HC.Manager -> Config -> Text -> Int -> Double -> Double -> IO (Either Text Text)
callTritonGenerate mgr cfg prompt maxTokens temp topP = do
  let url = cfgTritonUrl cfg <> "/v2/models/ensemble/generate"
      body = encode $ object
        [ "text_input" .= prompt
        , "max_tokens" .= maxTokens
        , "temperature" .= temp
        , "top_p" .= topP
        , "stream" .= False
        ]
  
  initReq <- HC.parseRequest url
  let req = initReq
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyLBS body
        , HC.requestHeaders = [("Content-Type", "application/json")]
        }
  
  (do
    resp <- HC.httpLbs req mgr
    let status = HC.responseStatus resp
    if statusCode status == 200
      then case decode (HC.responseBody resp) of
        Just obj -> case parseMaybe (.: "text_output") obj of
          Just txt -> pure $ Right txt
          Nothing -> pure $ Left "Missing text_output in response"
        Nothing -> pure $ Left "Failed to parse Triton response"
      else pure $ Left $ "Triton error: " <> T.pack (show $ statusCode status))
    `catch` \(e :: SomeException) -> 
      pure $ Left $ "Connection error: " <> T.pack (show e)

-- Triton streaming
streamTritonGenerate :: HC.Manager -> Config -> Text -> Int -> Double -> Double 
                     -> (Text -> IO ()) -> IO (Either Text ())
streamTritonGenerate mgr cfg prompt maxTokens temp topP onChunk = do
  let url = cfgTritonUrl cfg <> "/v2/models/ensemble/generate_stream"
      body = encode $ object
        [ "text_input" .= prompt
        , "max_tokens" .= maxTokens
        , "temperature" .= temp
        , "top_p" .= topP
        , "stream" .= True
        ]
  
  initReq <- HC.parseRequest url
  let req = initReq
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyLBS body
        , HC.requestHeaders = [("Content-Type", "application/json")]
        }
  
  bufferRef <- newIORef ""
  
  (do
    HC.withResponse req mgr $ \resp -> do
      let status = HC.responseStatus resp
      if statusCode status == 200
        then do
          let loop = do
                rawChunk <- HC.brRead $ HC.responseBody resp
                unless (BS.null rawChunk) $ do
                  let chunkText = TE.decodeUtf8With (\_ _ -> Just '?') rawChunk
                  buffer <- readIORef bufferRef
                  let fullText = buffer <> chunkText
                      allLines = T.splitOn "\n" fullText
                      (completeLines, remainder) = case allLines of
                        [] -> ([], "")
                        xs -> (init xs, last xs)
                  writeIORef bufferRef remainder
                  mapM_ processLine completeLines
                  loop
          loop
          finalBuffer <- readIORef bufferRef
          unless (T.null finalBuffer) $ processLine finalBuffer
          pure $ Right ()
        else 
          pure $ Left $ "Triton error: " <> T.pack (show $ statusCode status))
    `catch` \(e :: SomeException) -> 
      pure $ Left $ "Stream error: " <> T.pack (show e)
  where
    processLine line
      | "data:" `T.isPrefixOf` line = do
          let jsonPart = T.strip $ T.drop 5 line
          case decode (LBS.fromStrict $ TE.encodeUtf8 jsonPart) of
            Just obj -> case parseMaybe (.: "text_output") obj of
              Just txt -> onChunk txt
              Nothing -> pure ()
            Nothing -> pure ()
      | otherwise = pure ()

-- SearXNG search
searchSearxng :: HC.Manager -> Text -> Text -> Int -> IO SearchResponse
searchSearxng mgr baseUrl query numResults = do
  let url = T.unpack baseUrl <> "/search?q=" <> T.unpack query <> "&format=json"
  result <- (do
    initReq <- HC.parseRequest url
    resp <- HC.httpLbs initReq mgr
    pure $ Right $ HC.responseBody resp)
    `catch` \(e :: SomeException) -> pure $ Left $ T.pack (show e)
  
  case result of
    Left err -> pure $ SearchResponse "error" query [] (Just err)
    Right body -> case decode body of
      Nothing -> pure $ SearchResponse "error" query [] (Just "Failed to parse SearXNG response")
      Just obj -> do
        let results = case KM.lookup "results" obj of
              Just (Array arr) -> take numResults
                [ SearchResult
                    { srTitle = getString r "title"
                    , srUrl = getString r "url"
                    , srContent = getString r "content"
                    }
                | r <- foldr (:) [] arr
                ]
              _ -> []
        pure $ SearchResponse "ok" query results Nothing
  where
    getString :: Value -> Text -> Text
    getString (Object o) key = case KM.lookup (fromText key) o of
      Just (String t) -> t
      _ -> ""
    getString _ _ = ""

-- Jina Reader
readJina :: HC.Manager -> Text -> IO ReadUrlResponse
readJina mgr url = do
  let jinaUrl = "https://r.jina.ai/" <> T.unpack url
  result <- (do
    initReq <- HC.parseRequest jinaUrl
    let req = initReq { HC.requestHeaders = [("Accept", "application/json")] }
    resp <- HC.httpLbs req mgr
    pure $ Right $ HC.responseBody resp)
    `catch` \(e :: SomeException) -> pure $ Left $ T.pack (show e)
  
  case result of
    Left err -> pure $ ReadUrlResponse "error" url Nothing "" (Just err)
    Right body -> case decode body of
      Nothing -> pure $ ReadUrlResponse "ok" url Nothing (TE.decodeUtf8 $ LBS.toStrict body) Nothing
      Just obj -> do
        let dataObj = case KM.lookup "data" obj of
              Just (Object d) -> d
              _ -> obj
            content = case KM.lookup "content" dataObj of
              Just (String c) -> c
              _ -> case KM.lookup "text" dataObj of
                Just (String t) -> t
                _ -> ""
            title = case KM.lookup "title" dataObj of
              Just (String t) -> Just t
              _ -> Nothing
        pure $ ReadUrlResponse "ok" url title content Nothing


-- ════════════════════════════════════════════════════════════════════════════════
-- WAI Application
-- ════════════════════════════════════════════════════════════════════════════════

app :: Config -> HC.Manager -> MetricsState -> Application
app cfg mgr metrics req respond = do
  startTime <- getCurrentTime
  let path = pathInfo req
      meth = requestMethod req
      clientIP = maybe "" TE.decodeUtf8 $ lookup "X-Forwarded-For" (requestHeaders req)
  
  case (meth, path) of
    -- Health check
    ("GET", ["health"]) -> 
      respond $ responseLBS status200 jsonHeaders $ encode $ object
        [ "status" .= ("ok" :: Text)
        , "triton" .= cfgTritonUrl cfg
        , "searxng_configured" .= maybe False (const True) (cfgSearxngUrl cfg)
        , "metrics_enabled" .= cfgMetricsEnabled cfg
        , "strip_thinking" .= cfgStripThinking cfg
        ]
    
    -- Models endpoint
    ("GET", ["v1", "models"]) -> do
      now <- round <$> getPOSIXTime
      let rsp = ModelsResponse "list" 
            [ModelInfo (cfgModelName cfg) "model" now "nvidia"]
      respond $ responseLBS status200 jsonHeaders (encode rsp)
    
    -- Chat completions
    ("POST", ["v1", "chat", "completions"]) -> 
      handleChatCompletions cfg mgr metrics startTime clientIP req respond
    
    -- ════════════════════════════════════════════════════════════════════════
    -- Tool endpoints
    -- ════════════════════════════════════════════════════════════════════════
    
    -- Search
    ("GET", ["tools", "search"]) -> do
      let q = getQueryParam "query" req
      case cfgSearxngUrl cfg of
        Nothing -> respond $ responseLBS status200 jsonHeaders $ encode $
          SearchResponse "error" q [] (Just "SEARXNG_URL not configured")
        Just baseUrl -> do
          rsp <- searchSearxng mgr baseUrl q 5
          respond $ responseLBS status200 jsonHeaders (encode rsp)
    
    -- Code search (same as search but could filter engines)
    ("GET", ["tools", "code_search"]) -> do
      let q = getQueryParam "query" req
      case cfgSearxngUrl cfg of
        Nothing -> respond $ responseLBS status200 jsonHeaders $ encode $
          SearchResponse "error" q [] (Just "SEARXNG_URL not configured")
        Just baseUrl -> do
          rsp <- searchSearxng mgr baseUrl q 5
          respond $ responseLBS status200 jsonHeaders (encode rsp)
    
    -- Read URL
    ("GET", ["tools", "read_url"]) -> do
      let url = getQueryParam "url" req
      rsp <- readJina mgr url
      respond $ responseLBS status200 jsonHeaders (encode rsp)
    
    -- Coeffects manifest
    ("GET", ["coeffects", "manifest"]) ->
      respond $ responseLBS status200 jsonHeaders $ encode $ object
        [ "tools" .= object
            [ "/v1/chat/completions" .= object ["net" .= ("readWrite" :: Text), "gpu" .= ("read" :: Text)]
            , "/tools/search" .= object ["net" .= ("read" :: Text)]
            , "/tools/code_search" .= object ["net" .= ("read" :: Text)]
            , "/tools/read_url" .= object ["net" .= ("read" :: Text)]
            ]
        , "algebra" .= ("Coeffect semiring: (R, join, 0, meet, 1)" :: Text)
        ]
    
    -- 404
    _ -> respond $ responseLBS status404 jsonHeaders 
           "{\"error\":\"Not found\"}"
  where
    jsonHeaders = [("Content-Type", "application/json")]
    
    -- Helper to extract query parameter as Text
    getQueryParam :: BS.ByteString -> Request -> Text
    getQueryParam key r = 
      case lookup key (queryString r) of
        Just (Just v) -> TE.decodeUtf8 v
        _ -> ""

handleChatCompletions :: Config -> HC.Manager -> MetricsState -> UTCTime -> Text 
                      -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleChatCompletions cfg mgr metrics startTime clientIP req respond = do
  body <- strictRequestBody req
  let bytesIn = fromIntegral $ LBS.length body
  
  case decode body of
    Nothing -> 
      respond $ responseLBS status400 jsonHeaders 
        "{\"error\":{\"message\":\"Invalid JSON in request body\",\"type\":\"invalid_request_error\"}}"
    
    Just chatReq -> do
      let prompt = applyChatTemplate (reqMessages chatReq)
          -- Default 4096 to handle Qwen3's thinking mode which needs ~200-400 tokens for reasoning
          maxTokens = fromMaybe 4096 (reqMaxTokens chatReq)
          temp = fromMaybe 0.7 (reqTemperature chatReq)
          topP = fromMaybe 0.9 (reqTopP chatReq)
          isStreaming = fromMaybe False (reqStream chatReq)
          tokensIn = estimateTokens prompt
      
      reqId <- ("chatcmpl-" <>) . T.take 8 . toText <$> nextRandom
      now <- round <$> getPOSIXTime
      
      if isStreaming
        then handleStreamingRequest cfg mgr metrics startTime clientIP reqId now 
               prompt maxTokens temp topP tokensIn bytesIn respond
        else handleNonStreamingRequest cfg mgr metrics startTime clientIP chatReq reqId now 
               prompt maxTokens temp topP tokensIn bytesIn respond
  where
    jsonHeaders = [("Content-Type", "application/json")]

handleNonStreamingRequest :: Config -> HC.Manager -> MetricsState -> UTCTime -> Text
                          -> ChatRequest -> Text -> Int -> Text -> Int -> Double -> Double 
                          -> Int -> Int -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleNonStreamingRequest cfg mgr metrics startTime clientIP chatReq reqId now 
                          prompt maxTokens temp topP tokensIn bytesIn respond = do
  result <- callTritonGenerate mgr cfg prompt maxTokens temp topP
  endTime <- getCurrentTime
  let latencyMs = realToFrac (diffUTCTime endTime startTime) * 1000
  
  case result of
    Left err -> do
      recordMetrics metrics $ RequestMetrics
        { rmTimestamp = startTime, rmRequestId = reqId, rmEndpoint = "/v1/chat/completions"
        , rmModel = reqModel chatReq, rmTokensIn = tokensIn, rmTokensOut = 0
        , rmBytesIn = bytesIn, rmBytesOut = 0, rmLatencyMs = latencyMs
        , rmTtftMs = Nothing, rmStatusCode = 502, rmStream = False, rmClientIP = clientIP
        }
      respond $ responseLBS status502 jsonHeaders $ encode $ object
        [ "error" .= object ["message" .= err, "type" .= ("backend_error" :: Text)] ]
    
    Right output -> do
      -- Extract assistant response, conditionally stripping think blocks
      let extracted = CT.extractAssistantResponse output
          cleanOutput = if cfgStripThinking cfg 
                        then CT.stripThinking extracted
                        else extracted
          tokensOut = estimateTokens cleanOutput
          usage = UsageInfo tokensIn tokensOut (tokensIn + tokensOut)
          rsp = ChatResponse
            { respId = reqId
            , respObject = "chat.completion"
            , respCreated = now
            , respModel = reqModel chatReq
            , respChoices = [ChatChoice 0 (ChatMessage "assistant" (Just cleanOutput)) "stop"]
            , respUsage = usage
            }
          respBody = encode rsp
          bytesOut = fromIntegral $ LBS.length respBody
      
      recordMetrics metrics $ RequestMetrics
        { rmTimestamp = startTime, rmRequestId = reqId, rmEndpoint = "/v1/chat/completions"
        , rmModel = reqModel chatReq, rmTokensIn = tokensIn, rmTokensOut = tokensOut
        , rmBytesIn = bytesIn, rmBytesOut = bytesOut, rmLatencyMs = latencyMs
        , rmTtftMs = Nothing, rmStatusCode = 200, rmStream = False, rmClientIP = clientIP
        }
      respond $ responseLBS status200 jsonHeaders respBody
  where
    jsonHeaders = [("Content-Type", "application/json")]

handleStreamingRequest :: Config -> HC.Manager -> MetricsState -> UTCTime -> Text
                       -> Text -> Int -> Text -> Int -> Double -> Double 
                       -> Int -> Int -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleStreamingRequest cfg mgr metrics startTime clientIP reqId now 
                       prompt maxTokens temp topP tokensIn bytesIn respond = do
  
  -- Track metrics during streaming
  tokensOutRef <- newIORef (0 :: Int)
  bytesOutRef <- newIORef (0 :: Int)
  ttftRef <- newIORef (Nothing :: Maybe Double)
  
  respond $ responseStream status200 sseHeaders $ \write flush -> do
    -- First chunk with role
    let firstChunk = StreamChunk reqId "chat.completion.chunk" now (cfgModelName cfg)
          [StreamChoice 0 (ChunkDelta Nothing (Just "assistant")) Nothing]
        firstBytes = "data: " <> LBS.toStrict (encode firstChunk) <> "\n\n"
    write $ Builder.byteString firstBytes
    modifyIORef' bytesOutRef (+ BS.length firstBytes)
    flush
    
    -- Think filter (only used when stripping is enabled)
    filterRef <- newIORef CT.initThinkFilter
    
    let sendDelta :: Text -> IO ()
        sendDelta delta = unless (T.null delta) $ do
          -- Record TTFT on first content
          ttft <- readIORef ttftRef
          when (ttft == Nothing) $ do
            firstTokenTime <- getCurrentTime
            let ttftMs = realToFrac (diffUTCTime firstTokenTime startTime) * 1000
            writeIORef ttftRef (Just ttftMs)
          
          modifyIORef' tokensOutRef (+ estimateTokens delta)
          let chunk = StreamChunk reqId "chat.completion.chunk" now (cfgModelName cfg)
                [StreamChoice 0 (ChunkDelta (Just delta) Nothing) Nothing]
              chunkBytes = "data: " <> LBS.toStrict (encode chunk) <> "\n\n"
          write $ Builder.byteString chunkBytes
          modifyIORef' bytesOutRef (+ BS.length chunkBytes)
          flush
    
    _ <- streamTritonGenerate mgr cfg prompt maxTokens temp topP $ \tokenDelta -> do
      if cfgStripThinking cfg
        then do
          -- Use think filter to strip <think>...</think> blocks
          tf <- readIORef filterRef
          let CT.ThinkResult tf' output = CT.feedThinkFilter tf tokenDelta
          writeIORef filterRef tf'
          sendDelta output
        else
          -- Pass through unfiltered
          sendDelta tokenDelta
    
    -- Finalize (flush any buffered content from filter)
    when (cfgStripThinking cfg) $ do
      finalFilter <- readIORef filterRef
      let finalOutput = CT.finalizeThinkFilter finalFilter
      sendDelta finalOutput
    
    -- Final chunk
    let finalChunk = StreamChunk reqId "chat.completion.chunk" now (cfgModelName cfg)
          [StreamChoice 0 (ChunkDelta Nothing Nothing) (Just "stop")]
        finalBytes = "data: " <> LBS.toStrict (encode finalChunk) <> "\n\ndata: [DONE]\n\n"
    write $ Builder.byteString finalBytes
    modifyIORef' bytesOutRef (+ BS.length finalBytes)
    flush
    
    -- Record metrics
    endTime <- getCurrentTime
    tokensOut <- readIORef tokensOutRef
    bytesOut <- readIORef bytesOutRef
    ttft <- readIORef ttftRef
    let latencyMs = realToFrac (diffUTCTime endTime startTime) * 1000
    
    recordMetrics metrics $ RequestMetrics
      { rmTimestamp = startTime, rmRequestId = reqId, rmEndpoint = "/v1/chat/completions"
      , rmModel = cfgModelName cfg, rmTokensIn = tokensIn, rmTokensOut = tokensOut
      , rmBytesIn = bytesIn, rmBytesOut = bytesOut, rmLatencyMs = latencyMs
      , rmTtftMs = ttft, rmStatusCode = 200, rmStream = True, rmClientIP = clientIP
      }
  where
    sseHeaders = 
      [ ("Content-Type", "text/event-stream")
      , ("Cache-Control", "no-cache")
      , ("Connection", "keep-alive")
      , ("X-Accel-Buffering", "no")
      ]


-- ════════════════════════════════════════════════════════════════════════════════
-- Main
-- ════════════════════════════════════════════════════════════════════════════════

main :: IO ()
main = do
  cfg <- loadConfig
  mgr <- createManager
  metrics <- newMetricsState cfg mgr
  
  putStrLn $ replicate 80 '='
  putStrLn $ "  AI Gateway (Haskell/Warp)"
  putStrLn $ replicate 80 '='
  putStrLn ""
  putStrLn $ "  Model: " <> T.unpack (cfgModelName cfg)
  putStrLn $ "  Backend: " <> cfgTritonUrl cfg
  putStrLn ""
  putStrLn "  OpenAI Endpoints:"
  putStrLn $ "    http://localhost:" <> show (cfgPort cfg) <> "/v1/chat/completions"
  putStrLn $ "    http://localhost:" <> show (cfgPort cfg) <> "/v1/models"
  putStrLn ""
  putStrLn "  Tool Endpoints:"
  putStrLn $ "    http://localhost:" <> show (cfgPort cfg) <> "/tools/search?query=..."
  putStrLn $ "    http://localhost:" <> show (cfgPort cfg) <> "/tools/code_search?query=..."
  putStrLn $ "    http://localhost:" <> show (cfgPort cfg) <> "/tools/read_url?url=..."
  putStrLn ""
  putStrLn "  Configuration:"
  putStrLn $ "    SEARXNG_URL: " <> maybe "NOT SET" T.unpack (cfgSearxngUrl cfg)
  putStrLn $ "    STRIP_THINKING: " <> if cfgStripThinking cfg then "yes (hide <think> blocks)" else "no (show reasoning)"
  putStrLn $ "    METRICS_ENABLED: " <> if cfgMetricsEnabled cfg then "yes" else "no"
  when (cfgMetricsEnabled cfg) $ do
    putStrLn $ "    CLICKHOUSE_URL: " <> T.unpack (cfgClickhouseUrl cfg)
    putStrLn $ "    CLICKHOUSE_DATABASE: " <> T.unpack (cfgClickhouseDb cfg)
  putStrLn ""
  putStrLn $ replicate 80 '='
  hFlush stdout
  
  runSettings (settings cfg) (app cfg mgr metrics)
  where
    settings cfg' = setPort (cfgPort cfg')
                  $ setHost "*"
                  $ setOnException (\_ e -> hPutStrLn stderr $ "Exception: " <> show e)
                  $ defaultSettings
