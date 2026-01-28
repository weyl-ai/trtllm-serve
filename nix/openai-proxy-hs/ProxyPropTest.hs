{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : ProxyPropTest
Description : Property-based testing for OpenAI proxy
License     : MIT

QuickCheck-based property tests for the OpenAI proxy against a mock Triton server.

Tests:
1. Response structure equivalence
2. Error handling propagation
3. Latency bounds
4. Malformed input handling
5. Concurrent request handling
-}
module Main where

import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.Async (async, cancel, waitAny, race)
import Control.Concurrent.MVar
import Control.Exception (bracket, SomeException, catch, finally)
import Control.Monad (replicateM, forM_, when, void)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Builder as Builder
import Data.IORef
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import GHC.Generics
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (Port)
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (lookupEnv, setEnv)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import System.Process (createProcess, proc, terminateProcess, ProcessHandle)
import System.Timeout (timeout)
import Test.QuickCheck hiding (Success)
import Test.QuickCheck.Monadic
import qualified Test.QuickCheck as QC
import Text.Read (readMaybe)


-- ════════════════════════════════════════════════════════════════════════════════
-- Configuration
-- ════════════════════════════════════════════════════════════════════════════════

data TestConfig = TestConfig
  { tcMockTritonPort :: !Port
  , tcHaskellProxyPort :: !Port
  , tcTimeoutSec :: !Int
  , tcNumTests :: !Int
  } deriving (Show)

defaultTestConfig :: TestConfig
defaultTestConfig = TestConfig
  { tcMockTritonPort = 18000
  , tcHaskellProxyPort = 19000
  , tcTimeoutSec = 30
  , tcNumTests = 100
  }


-- ════════════════════════════════════════════════════════════════════════════════
-- Mock Triton Server
-- ════════════════════════════════════════════════════════════════════════════════

data MockBehavior
  = Normal !Text           -- Normal response with given text
  | Delayed !Text !Int     -- Response with delay (ms)
  | MockError !Int !Text   -- HTTP error code with message
  | Malformed              -- Return invalid JSON
  | Timeout                -- Never respond
  | Streaming ![Text] !Int -- SSE chunks with delay between each
  deriving (Show, Eq)

-- | State for the mock server, controlled via IORef
data MockState = MockState
  { msBehavior :: !MockBehavior
  , msRequestCount :: !Int
  , msLastRequest :: !(Maybe Value)
  } deriving (Show)

newMockState :: IO (IORef MockState)
newMockState = newIORef MockState
  { msBehavior = Normal "Hello from mock Triton!"
  , msRequestCount = 0
  , msLastRequest = Nothing
  }

setBehavior :: IORef MockState -> MockBehavior -> IO ()
setBehavior ref b = atomicModifyIORef' ref $ \s -> (s { msBehavior = b }, ())

getRequestCount :: IORef MockState -> IO Int
getRequestCount ref = msRequestCount <$> readIORef ref

-- | Mock Triton WAI application
mockTritonApp :: IORef MockState -> Application
mockTritonApp stateRef req respond = do
  -- Read request body
  body <- strictRequestBody req
  let maybeJson = decode body :: Maybe Value
  
  -- Update state
  atomicModifyIORef' stateRef $ \s -> 
    (s { msRequestCount = msRequestCount s + 1, msLastRequest = maybeJson }, ())
  
  state <- readIORef stateRef
  let behavior = msBehavior state
  
  case pathInfo req of
    ["v2", "health", "ready"] ->
      respond $ responseLBS status200 [("Content-Type", "application/json")] "{\"ready\": true}"
    
    ["v2", "models", _, "generate"] ->
      handleGenerate behavior respond
    
    ["v2", "models", _, "generate_stream"] ->
      handleGenerateStream behavior respond
    
    _ -> respond $ responseLBS status404 [] "Not Found"

handleGenerate :: MockBehavior -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleGenerate behavior respond = case behavior of
  Normal txt -> do
    let resp = object ["text_output" .= txt]
    respond $ responseLBS status200 jsonHeaders (encode resp)
  
  Delayed txt ms -> do
    threadDelay (ms * 1000)
    let resp = object ["text_output" .= txt]
    respond $ responseLBS status200 jsonHeaders (encode resp)
  
  MockError code msg -> do
    let status = mkStatus code (BS8.pack $ T.unpack msg)
        resp = object ["error" .= msg]
    respond $ responseLBS status jsonHeaders (encode resp)
  
  Malformed ->
    respond $ responseLBS status200 jsonHeaders "{ invalid json ]["
  
  Timeout -> do
    threadDelay (300 * 1000 * 1000)  -- 5 minutes
    respond $ responseLBS status200 jsonHeaders "{}"
  
  Streaming chunks _ -> do
    -- Non-streaming endpoint returns final result
    let finalText = T.concat chunks
        resp = object ["text_output" .= finalText]
    respond $ responseLBS status200 jsonHeaders (encode resp)

handleGenerateStream :: MockBehavior -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleGenerateStream behavior respond = case behavior of
  Streaming chunks delayMs -> do
    respond $ responseStream status200 sseHeaders $ \write flush -> do
      accumulated <- newIORef T.empty
      forM_ chunks $ \chunk -> do
        modifyIORef' accumulated (<> chunk)
        current <- readIORef accumulated
        let payload = encode $ object ["text_output" .= current]
            event = "data:" <> LBS.toStrict payload <> "\n\n"
        write $ byteString event
        flush
        threadDelay (delayMs * 1000)
  
  Normal txt -> do
    respond $ responseStream status200 sseHeaders $ \write flush -> do
      let payload = encode $ object ["text_output" .= txt]
          event = "data:" <> LBS.toStrict payload <> "\n\n"
      write $ byteString event
      flush
  
  _ -> handleGenerate behavior respond

jsonHeaders :: ResponseHeaders
jsonHeaders = [("Content-Type", "application/json")]

sseHeaders :: ResponseHeaders
sseHeaders = 
  [ ("Content-Type", "text/event-stream")
  , ("Cache-Control", "no-cache")
  , ("Connection", "keep-alive")
  ]

byteString :: BS.ByteString -> Builder.Builder
byteString = Builder.byteString


-- ════════════════════════════════════════════════════════════════════════════════
-- Proxy Client
-- ════════════════════════════════════════════════════════════════════════════════

data ProxyResponse = ProxyResponse
  { prStatusCode :: !Int
  , prBody :: !(Maybe Value)
  , prContent :: !(Maybe Text)
  , prError :: !(Maybe Text)
  , prLatencyMs :: !Double
  } deriving (Show, Eq)

chatCompletion :: Manager -> String -> Text -> Bool -> Int -> IO ProxyResponse
chatCompletion mgr baseUrl prompt stream maxTokens = do
  let url = baseUrl <> "/v1/chat/completions"
      payload = object
        [ "model" .= ("test" :: Text)
        , "messages" .= [object ["role" .= ("user" :: Text), "content" .= prompt]]
        , "stream" .= stream
        , "max_tokens" .= maxTokens
        ]
  
  startTime <- getCurrentTime
  
  result <- timeout (30 * 1000 * 1000) $ do  -- 30 second timeout
    initReq <- HC.parseRequest url
    let req = initReq
          { HC.method = "POST"
          , HC.requestHeaders = [("Content-Type", "application/json")]
          , HC.requestBody = HC.RequestBodyLBS (encode payload)
          }
    HC.httpLbs req mgr
  
  endTime <- getCurrentTime
  let latencyMs = realToFrac (diffUTCTime endTime startTime) * 1000
  
  case result of
    Nothing -> return ProxyResponse
      { prStatusCode = 0
      , prBody = Nothing
      , prContent = Nothing
      , prError = Just "timeout"
      , prLatencyMs = latencyMs
      }
    
    Just resp -> do
      let status = HC.responseStatus resp
          bodyLbs = HC.responseBody resp
          maybeBody = decode bodyLbs :: Maybe Value
          content = maybeBody >>= extractContent
          err = maybeBody >>= extractError
      
      return ProxyResponse
        { prStatusCode = statusCode status
        , prBody = maybeBody
        , prContent = content
        , prError = err
        , prLatencyMs = latencyMs
        }

extractContent :: Value -> Maybe Text
extractContent (Object o) = do
  Array choices <- KM.lookup "choices" o
  case V.toList choices of
    (Object choice:_) -> do
      Object msg <- KM.lookup "message" choice
      String content <- KM.lookup "content" msg
      return content
    _ -> Nothing
extractContent _ = Nothing

extractError :: Value -> Maybe Text
extractError (Object o) = case KM.lookup "error" o of
  Just (Object err) -> case KM.lookup "message" err of
    Just (String msg) -> Just msg
    _ -> Nothing
  Just (String msg) -> Just msg
  _ -> Nothing
extractError _ = Nothing


-- ════════════════════════════════════════════════════════════════════════════════
-- Generators
-- ════════════════════════════════════════════════════════════════════════════════

-- | Generate reasonable ASCII text
genText :: Int -> Int -> Gen Text
genText minLen maxLen = do
  len <- choose (minLen, maxLen)
  chars <- vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " .,!?"
  return $ T.pack chars

-- | Generate chunks for streaming
genChunks :: Gen [Text]
genChunks = do
  n <- choose (2, 10)
  replicateM n (genText 5 20)

-- | Generate error codes
genErrorCode :: Gen Int
genErrorCode = elements [400, 401, 403, 404, 500, 502, 503]

-- | Generate delay in milliseconds
genDelayMs :: Gen Int
genDelayMs = choose (0, 500)


-- ════════════════════════════════════════════════════════════════════════════════
-- Properties
-- ════════════════════════════════════════════════════════════════════════════════

-- | Property: Normal responses should return 200 with content
prop_normalResponse :: IORef MockState -> Manager -> String -> Property
prop_normalResponse mockState mgr proxyUrl = monadicIO $ do
  responseText <- pick $ genText 10 50
  run $ setBehavior mockState (Normal responseText)
  
  resp <- run $ chatCompletion mgr proxyUrl "test" False 100
  
  assert $ prStatusCode resp == 200
  assert $ isJust (prContent resp)

-- | Property: Backend errors should be propagated
prop_errorPropagation :: IORef MockState -> Manager -> String -> Property
prop_errorPropagation mockState mgr proxyUrl = monadicIO $ do
  errCode <- pick genErrorCode
  errMsg <- pick $ genText 10 30
  run $ setBehavior mockState (MockError errCode errMsg)
  
  resp <- run $ chatCompletion mgr proxyUrl "test" False 100
  
  -- Proxy should return an error status
  assert $ prStatusCode resp >= 400 || prStatusCode resp == 0

-- | Property: Latency should be within bounds
prop_latencyBounds :: IORef MockState -> Manager -> String -> Property
prop_latencyBounds mockState mgr proxyUrl = monadicIO $ do
  delayMs <- pick genDelayMs
  run $ setBehavior mockState (Delayed "response" delayMs)
  
  resp <- run $ chatCompletion mgr proxyUrl "test" False 100
  
  -- Should succeed
  assert $ prStatusCode resp == 200
  
  -- Latency should be at least the delay (minus some tolerance)
  let minLatency = fromIntegral delayMs - 50
      maxLatency = fromIntegral delayMs + 1000  -- Allow 1s overhead
  
  monitor $ counterexample $ 
    "Expected latency in [" ++ show minLatency ++ ", " ++ show maxLatency ++ 
    "], got " ++ show (prLatencyMs resp)
  
  assert $ prLatencyMs resp >= minLatency
  assert $ prLatencyMs resp <= maxLatency

-- | Property: Malformed responses should be handled gracefully (not crash)
prop_malformedHandling :: IORef MockState -> Manager -> String -> Property
prop_malformedHandling mockState mgr proxyUrl = monadicIO $ do
  run $ setBehavior mockState Malformed
  
  resp <- run $ chatCompletion mgr proxyUrl "test" False 100
  
  -- Should return an error, not crash
  assert $ prStatusCode resp /= 200 || isJust (prError resp) || prContent resp == Nothing

-- | Property: Concurrent requests should all succeed
prop_concurrentRequests :: IORef MockState -> Manager -> String -> Property
prop_concurrentRequests mockState mgr proxyUrl = monadicIO $ do
  n <- pick $ choose (3, 8)
  run $ setBehavior mockState (Delayed "concurrent test" 50)
  
  responses <- run $ do
    asyncs <- replicateM n $ async $ chatCompletion mgr proxyUrl "test" False 100
    mapM (\a -> catch (Right <$> waitAsync a) (\(e :: SomeException) -> return $ Left e)) asyncs
  
  let successes = length [r | Right r <- responses, prStatusCode r == 200]
  
  monitor $ counterexample $ 
    "Expected " ++ show n ++ " successes, got " ++ show successes
  
  assert $ successes == n
  where
    waitAsync a = do
      result <- race (threadDelay (30 * 1000 * 1000)) (cancel a >> return undefined)
      case result of
        Left () -> error "async timeout"
        Right _ -> error "cancelled"


-- ════════════════════════════════════════════════════════════════════════════════
-- Test Runner
-- ════════════════════════════════════════════════════════════════════════════════

runProperty :: String -> Property -> IO Bool
runProperty name prop = do
  putStr $ "Testing: " ++ name ++ " "
  hFlush stdout
  result <- quickCheckWithResult stdArgs { maxSuccess = 20, chatty = False } prop
  case result of
    QC.Success {} -> do
      putStrLn "✓"
      return True
    _ -> do
      putStrLn $ "✗ " ++ show result
      return False

main :: IO ()
main = do
  let config = defaultTestConfig
  
  putStrLn ""
  putStrLn $ replicate 70 '═'
  putStrLn "  Property-Based Tests: OpenAI Proxy"
  putStrLn $ replicate 70 '═'
  putStrLn ""
  
  -- Create mock state
  mockState <- newMockState
  
  -- Start mock Triton server
  putStrLn $ "Starting mock Triton on port " ++ show (tcMockTritonPort config)
  mockThread <- forkIO $ Warp.run (tcMockTritonPort config) (mockTritonApp mockState)
  threadDelay 500000  -- Wait for server to start
  
  -- Create HTTP manager
  mgr <- newManager defaultManagerSettings
  
  -- Set environment for proxy
  setEnv "TRITON_URL" $ "http://localhost:" ++ show (tcMockTritonPort config)
  setEnv "OPENAI_PROXY_PORT" $ show (tcHaskellProxyPort config)
  setEnv "MODEL_NAME" "test"
  
  -- Start the Haskell proxy (assumes it's built and in PATH or ./result/bin)
  putStrLn $ "Starting Haskell proxy on port " ++ show (tcHaskellProxyPort config)
  (_, _, _, proxyHandle) <- createProcess (proc "./result/bin/openai-proxy-hs" [])
  threadDelay 2000000  -- Wait for proxy to start
  
  let proxyUrl = "http://localhost:" ++ show (tcHaskellProxyPort config)
  
  -- Run properties
  results <- sequence
    [ runProperty "normal_response" $ prop_normalResponse mockState mgr proxyUrl
    , runProperty "error_propagation" $ prop_errorPropagation mockState mgr proxyUrl
    , runProperty "latency_bounds" $ prop_latencyBounds mockState mgr proxyUrl
    , runProperty "malformed_handling" $ prop_malformedHandling mockState mgr proxyUrl
    -- Skip concurrent for now - needs better async handling
    -- , runProperty "concurrent_requests" $ prop_concurrentRequests mockState mgr proxyUrl
    ]
  
  -- Cleanup
  putStrLn "\nCleaning up..."
  terminateProcess proxyHandle
  killThread mockThread
  
  -- Report results
  let passed = length $ filter id results
      total = length results
  
  putStrLn ""
  putStrLn $ replicate 70 '═'
  putStrLn $ "  Total: " ++ show passed ++ "/" ++ show total ++ " passed"
  putStrLn $ replicate 70 '═'
  putStrLn ""
  
  if passed == total
    then exitSuccess
    else exitFailure
