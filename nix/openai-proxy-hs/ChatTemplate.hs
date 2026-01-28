{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

{-|
Module      : ChatTemplate
Description : Megaparsec grammar for Qwen3 chat format
License     : MIT

Parses and renders Qwen3/ChatML-style chat templates.

Format:
  <|im_start|>system
  You are a helpful assistant.<|im_end|>
  <|im_start|>user
  Hello<|im_end|>
  <|im_start|>assistant
  Hi there!<|im_end|>

Special tokens:
  <|im_start|>  - Start of message
  <|im_end|>    - End of message
  <|endoftext|> - End of sequence
  
Qwen3 thinking mode:
  <think>...</think>  - Model's internal reasoning (can be stripped)
  /think or /no_think - User control over thinking mode

Incremental streaming:
  ThinkFilter - State machine for filtering <think> blocks from token stream
-}
module ChatTemplate 
  ( -- * Types
    Role(..)
  , Message(..)
  , Chat(..)
    -- * Parsing
  , parseChat
  , Parser
    -- * Rendering
  , renderRole
  , renderMessage
  , renderChat
  , renderChatComplete
    -- * Think block handling
  , stripThinking
  , extractThinking
    -- * Incremental streaming filter
  , ThinkFilter(..)
  , ThinkResult(..)
  , initThinkFilter
  , feedThinkFilter
  , finalizeThinkFilter
    -- * OpenAI conversion
  , fromOpenAI
  , toOpenAI
    -- * Response extraction
  , extractAssistantResponse
    -- * Special tokens
  , imStart, imEnd, endOfText
  , thinkStart, thinkEnd
    -- * Testing
  , testParse
  , testThinkFilter
  ) where

import Control.Applicative ((<|>), optional)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char


-- ════════════════════════════════════════════════════════════════════════════════
-- Types
-- ════════════════════════════════════════════════════════════════════════════════

-- | Chat message role
data Role 
  = System 
  | User 
  | Assistant 
  | Tool
  deriving (Show, Eq, Ord)

-- | A single chat message
data Message = Message
  { msgRole    :: !Role
  , msgContent :: !Text
  , msgThought :: !(Maybe Text)  -- Extracted <think> content
  } deriving (Show, Eq)

-- | A complete chat conversation
data Chat = Chat
  { chatMessages :: ![Message]
  , chatPending  :: !(Maybe Role)  -- Role waiting for completion
  } deriving (Show, Eq)

-- | Parser type
type Parser = Parsec Void Text


-- ════════════════════════════════════════════════════════════════════════════════
-- Special Tokens
-- ════════════════════════════════════════════════════════════════════════════════

-- | Special token markers
imStart, imEnd, endOfText :: Text
imStart   = "<|im_start|>"
imEnd     = "<|im_end|>"
endOfText = "<|endoftext|>"

-- | Think tags for reasoning
thinkStart, thinkEnd :: Text
thinkStart = "<think>"
thinkEnd   = "</think>"


-- ════════════════════════════════════════════════════════════════════════════════
-- Parsers
-- ════════════════════════════════════════════════════════════════════════════════

-- | Parse a special token
pToken :: Text -> Parser Text
pToken t = string t

-- | Parse role name
pRole :: Parser Role
pRole = choice
  [ System    <$ string "system"
  , User      <$ string "user"  
  , Assistant <$ string "assistant"
  , Tool      <$ string "tool"
  ]

-- | Parse content until end marker or EOF
-- Handles nested content correctly
pContent :: Parser Text
pContent = T.pack <$> manyTill anySingle endMarker
  where
    endMarker = lookAhead $ void (pToken imEnd) <|> void (pToken imStart) <|> eof

-- | Parse thinking block
pThink :: Parser Text
pThink = do
  _ <- pToken thinkStart
  content <- T.pack <$> manyTill anySingle (pToken thinkEnd)
  pure content

-- | Parse message content, extracting think blocks
pMessageContent :: Parser (Text, Maybe Text)
pMessageContent = do
  -- Try to parse leading whitespace
  _ <- many (char ' ' <|> char '\t')
  _ <- optional newline
  
  -- Check for think block first
  maybeThink <- optional (try pThink)
  
  -- Skip whitespace after think
  _ <- many (char ' ' <|> char '\t' <|> char '\n')
  
  -- Parse remaining content
  content <- pContent
  
  let cleanContent = T.strip content
  pure (cleanContent, maybeThink)

-- | Parse a single message (must have im_end)
pMessage :: Parser Message
pMessage = do
  _ <- pToken imStart
  role <- pRole
  _ <- optional (char '\n')
  (content, thought) <- pMessageContent
  _ <- pToken imEnd  -- Required - distinguishes complete message from pending
  _ <- many (char '\n' <|> char ' ')
  pure Message
    { msgRole = role
    , msgContent = content
    , msgThought = thought
    }

-- | Parse assistant generation prompt (no content yet)
pAssistantPrompt :: Parser Role
pAssistantPrompt = do
  _ <- pToken imStart
  role <- pRole
  _ <- optional (char '\n')
  pure role

-- | Parse complete chat
pChat :: Parser Chat
pChat = do
  _ <- many (char '\n' <|> char ' ')
  msgs <- many (try pMessage)
  pending <- optional (try pAssistantPrompt)
  _ <- many (char '\n' <|> char ' ')
  eof
  pure Chat
    { chatMessages = msgs
    , chatPending = pending
    }

-- | Parse chat, returning error message on failure
parseChat :: Text -> Either String Chat
parseChat input = case parse pChat "chat" input of
  Left err -> Left $ errorBundlePretty err
  Right chat -> Right chat


-- ════════════════════════════════════════════════════════════════════════════════
-- Renderers  
-- ════════════════════════════════════════════════════════════════════════════════

-- | Render role to text
renderRole :: Role -> Text
renderRole = \case
  System    -> "system"
  User      -> "user"
  Assistant -> "assistant"
  Tool      -> "tool"

-- | Render a message
renderMessage :: Message -> Text
renderMessage Message{..} = T.concat
  [ imStart
  , renderRole msgRole
  , "\n"
  , maybe "" (\t -> thinkStart <> t <> thinkEnd <> "\n") msgThought
  , msgContent
  , imEnd
  , "\n"
  ]

-- | Render chat for model input (with generation prompt)
renderChat :: Chat -> Text
renderChat Chat{..} = T.concat $
  map renderMessage chatMessages ++
  [ imStart
  , renderRole (fromMaybe Assistant chatPending)
  , "\n"
  ]

-- | Render chat without generation prompt
renderChatComplete :: Chat -> Text
renderChatComplete Chat{..} = T.concat $ map renderMessage chatMessages


-- ════════════════════════════════════════════════════════════════════════════════
-- Utilities
-- ════════════════════════════════════════════════════════════════════════════════

-- | Strip thinking from assistant response
-- Handles both closed <think>...</think> and unclosed <think>... (truncated)
stripThinking :: Text -> Text
stripThinking txt = case parse pStripThink "strip" txt of
  Left _ -> txt
  Right cleaned -> cleaned
  where
    pStripThink :: Parser Text
    pStripThink = do
      parts <- many $ choice
        [ try $ do
            -- Closed think block: <think>...</think>
            _ <- pToken thinkStart
            _ <- manyTill anySingle (pToken thinkEnd)
            _ <- many (char '\n' <|> char ' ')
            pure ""
        , try $ do
            -- Unclosed think block: <think>... (rest of text is thinking)
            _ <- pToken thinkStart
            _ <- many anySingle  -- Consume everything after unclosed <think>
            eof
            pure ""
        , T.singleton <$> anySingle
        ]
      pure $ T.strip $ T.concat parts

-- | Extract thinking from assistant response
extractThinking :: Text -> Maybe Text
extractThinking txt = case parse pExtract "extract" txt of
  Left _ -> Nothing
  Right t -> if T.null t then Nothing else Just t
  where
    pExtract :: Parser Text
    pExtract = do
      _ <- manyTill anySingle (lookAhead (pToken thinkStart) <|> ("" <$ eof))
      optional pThink >>= \case
        Nothing -> pure ""
        Just t -> pure t

-- | Build chat from OpenAI-style messages
fromOpenAI :: [(Text, Text)] -> Chat
fromOpenAI msgs = Chat
  { chatMessages = map toMsg msgs
  , chatPending = Just Assistant
  }
  where
    toMsg (role, content) = Message
      { msgRole = parseRole role
      , msgContent = content
      , msgThought = Nothing
      }
    parseRole r = case T.toLower r of
      "system"    -> System
      "user"      -> User
      "assistant" -> Assistant
      "tool"      -> Tool
      _           -> User

-- | Convert to OpenAI-style messages (strips thinking)
toOpenAI :: Chat -> [(Text, Text)]
toOpenAI Chat{..} = map fromMsg chatMessages
  where
    fromMsg Message{..} = (renderRole msgRole, stripThinking msgContent)


-- ════════════════════════════════════════════════════════════════════════════════
-- Response Extraction
-- ════════════════════════════════════════════════════════════════════════════════

{-|
Extract assistant response from Triton output.

Triton echoes the full prompt, so output looks like:
  <|im_start|>system
  ...<|im_end|>
  <|im_start|>user
  ...<|im_end|>
  <|im_start|>assistant
  <think>...</think>
  Actual response here<|im_end|>

We want just "Actual response here" (after stripping think block).
-}

-- | Extract the assistant's response from Triton output (without stripping think blocks)
-- Returns content after the last "assistant\n" marker.
--
-- Triton may return output in two formats:
-- 1. With special tokens: <|im_start|>assistant\n...<|im_end|>
-- 2. Without special tokens (stripped): assistant\n...
--
-- We handle both cases.
extractAssistantResponse :: Text -> Text
extractAssistantResponse txt = case parse pExtractAssistant "extract" txt of
  Left _  -> T.strip txt  -- Parse failed, return trimmed original
  Right r -> T.strip r
  where
    pExtractAssistant :: Parser Text
    pExtractAssistant = do
      -- Find all assistant markers and take content after the last one
      segments <- many $ choice
        [ try $ do
            -- Case 1: With special tokens <|im_start|>assistant\n
            _ <- pToken imStart
            _ <- string "assistant"
            _ <- optional (char '\n')
            -- Consume until im_end or end
            content <- T.pack <$> manyTill anySingle 
              (lookAhead (void (pToken imEnd) <|> void (pToken imStart) <|> eof))
            _ <- optional (pToken imEnd)
            pure (Just content)
        , try $ do
            -- Case 2: Without special tokens - just "assistant\n" 
            -- Can be at start of line (after newline) or after other role markers
            _ <- optional (char '\n')
            _ <- string "assistant"
            _ <- char '\n'
            -- Consume until end or next role marker (no im_end tokens in this format)
            content <- T.pack <$> manyTill anySingle 
              (lookAhead (void (try (char '\n' >> string "user" >> char '\n')) 
                     <|> void (try (char '\n' >> string "system" >> char '\n'))
                     <|> eof))
            pure (Just content)
        , do
            -- Skip other content
            _ <- anySingle
            pure Nothing
        ]
      -- Return the last assistant content (the generated response)
      let assistantContents = [c | Just c <- segments]
      pure $ if null assistantContents
             then txt
             else last assistantContents


-- ════════════════════════════════════════════════════════════════════════════════
-- Incremental Streaming Think Filter
-- ════════════════════════════════════════════════════════════════════════════════

{-|
State machine for incrementally filtering <think>...</think> blocks from a
token stream. This is the correct way to handle streaming - we can't use
the batch parser because tokens arrive one at a time.

State transitions:
  
  OutsideThink ──<think>──> InsideThink ──</think>──> OutsideThink
       │                         │
       │ (other)                 │ (other)
       ▼                         ▼
    emit token               buffer (discard)

The tricky part: tags can be split across tokens!
  Token 1: "Hello <thi"
  Token 2: "nk>reason"
  Token 3: "ing</think>world"

We handle this by buffering potential partial tags.
-}

-- | State of the incremental think filter
data ThinkFilter = ThinkFilter
  { tfState  :: !ThinkState   -- ^ Current state
  , tfBuffer :: !Text         -- ^ Buffered text (potential partial tag)
  } deriving (Show, Eq)

-- | Filter state
data ThinkState
  = OutsideThink  -- ^ Not inside a think block, emitting content
  | InsideThink   -- ^ Inside <think>...</think>, discarding content
  deriving (Show, Eq)

-- | Result of feeding a token to the filter
data ThinkResult = ThinkResult
  { trFilter :: !ThinkFilter   -- ^ Updated filter state
  , trOutput :: !Text          -- ^ Text to emit (may be empty)
  } deriving (Show, Eq)

-- | Initialize filter. Qwen3 typically starts with <think>, so we begin outside.
initThinkFilter :: ThinkFilter
initThinkFilter = ThinkFilter OutsideThink ""

-- | Feed a token to the filter, get output and new state
feedThinkFilter :: ThinkFilter -> Text -> ThinkResult
feedThinkFilter (ThinkFilter state buffer) token =
  processBuffer (ThinkFilter state (buffer <> token))

-- | Process buffered content, emitting what we can
processBuffer :: ThinkFilter -> ThinkResult
processBuffer tf@(ThinkFilter state buffer)
  | T.null buffer = ThinkResult tf ""
  | otherwise = case state of
      OutsideThink -> processOutside tf
      InsideThink  -> processInside tf

-- | Process buffer when outside think block
-- We emit content until we see <think> or a potential partial <think>
processOutside :: ThinkFilter -> ThinkResult
processOutside (ThinkFilter _ buffer) =
  case findTagStart thinkStart buffer of
    -- Found complete <think> tag
    FoundComplete idx ->
      let before = T.take idx buffer
          after = T.drop (idx + T.length thinkStart) buffer
          -- Emit what's before, continue processing inside think
          ThinkResult tf' more = processBuffer (ThinkFilter InsideThink after)
      in ThinkResult tf' (before <> more)
    
    -- Found partial tag at end - buffer it, emit what's before
    FoundPartial idx ->
      let emit = T.take idx buffer
          keep = T.drop idx buffer
      in ThinkResult (ThinkFilter OutsideThink keep) emit
    
    -- No tag found - emit everything
    NotFound ->
      ThinkResult (ThinkFilter OutsideThink "") buffer

-- | Process buffer when inside think block
-- We discard content until we see </think>
processInside :: ThinkFilter -> ThinkResult
processInside (ThinkFilter _ buffer) =
  case findTagStart thinkEnd buffer of
    -- Found complete </think> tag
    FoundComplete idx ->
      let after = T.drop (idx + T.length thinkEnd) buffer
          -- Strip leading whitespace after think block
          trimmed = T.dropWhile (`elem` ['\n', '\r', ' ', '\t']) after
          -- Continue processing outside think
      in processBuffer (ThinkFilter OutsideThink trimmed)
    
    -- Found partial tag - keep buffering (still inside)
    FoundPartial idx ->
      let keep = T.drop idx buffer
      in ThinkResult (ThinkFilter InsideThink keep) ""
    
    -- No tag found - discard everything, stay inside
    NotFound ->
      ThinkResult (ThinkFilter InsideThink "") ""

-- | Result of searching for a tag
data TagSearch
  = FoundComplete !Int   -- ^ Complete tag found at index
  | FoundPartial !Int    -- ^ Partial tag at end, starting at index
  | NotFound             -- ^ No tag or partial tag
  deriving (Show, Eq)

-- | Search for a tag or partial tag in text
findTagStart :: Text -> Text -> TagSearch
findTagStart tag txt =
  case T.breakOn tag txt of
    (before, after)
      | not (T.null after) -> FoundComplete (T.length before)
      | otherwise -> 
          -- Check if text ends with a prefix of the tag
          case findPartialAtEnd tag txt of
            Just idx -> FoundPartial idx
            Nothing  -> NotFound

-- | Check if text ends with a prefix of the tag
-- Returns the index where the partial starts
findPartialAtEnd :: Text -> Text -> Maybe Int
findPartialAtEnd tag txt = go (T.length tag - 1)
  where
    go 0 = Nothing  -- Single char prefix not worth buffering
    go n =
      let prefix = T.take n tag
          suffix = T.takeEnd n txt
      in if prefix == suffix
         then Just (T.length txt - n)
         else go (n - 1)

-- | Finalize the filter - flush any remaining buffer
-- Called when stream ends
finalizeThinkFilter :: ThinkFilter -> Text
finalizeThinkFilter (ThinkFilter state buffer) =
  case state of
    OutsideThink -> buffer  -- Emit remaining (partial tag was false alarm)
    InsideThink  -> ""      -- Discard (unclosed think block)


-- ════════════════════════════════════════════════════════════════════════════════
-- Tests (inline examples)
-- ════════════════════════════════════════════════════════════════════════════════

-- | Example chat template
exampleChat :: Text
exampleChat = T.unlines
  [ "<|im_start|>system"
  , "You are a helpful assistant.<|im_end|>"
  , "<|im_start|>user"
  , "What is 2+2?<|im_end|>"
  , "<|im_start|>assistant"
  , "<think>"
  , "The user wants to know 2+2. This is basic arithmetic."
  , "</think>"
  , "The answer is 4.<|im_end|>"
  ]

-- | Example with pending generation
examplePending :: Text
examplePending = T.unlines
  [ "<|im_start|>system"
  , "You are helpful.<|im_end|>"
  , "<|im_start|>user"
  , "Hello<|im_end|>"
  , "<|im_start|>assistant"
  ]

-- | Test parser
testParse :: IO ()
testParse = do
  putStrLn "=== Testing complete chat ==="
  case parseChat exampleChat of
    Left err -> putStrLn $ "FAIL: " ++ err
    Right chat -> do
      putStrLn "OK: Parsed"
      print chat
      putStrLn "\nRe-rendered:"
      putStrLn $ T.unpack $ renderChatComplete chat
  
  putStrLn "\n=== Testing pending generation ==="
  case parseChat examplePending of
    Left err -> putStrLn $ "FAIL: " ++ err
    Right chat -> do
      putStrLn "OK: Parsed"
      print chat
      putStrLn "\nFor model input:"
      putStrLn $ T.unpack $ renderChat chat

  putStrLn "\n=== Testing strip thinking ==="
  let withThink = "<think>\nSome reasoning\n</think>\nThe answer is 42."
  putStrLn $ "Input: " ++ T.unpack withThink
  putStrLn $ "Stripped: " ++ T.unpack (stripThinking withThink)
  putStrLn $ "Extracted: " ++ show (extractThinking withThink)

  putStrLn "\n=== Testing incremental think filter ==="
  testThinkFilter

-- | Test the incremental think filter with various token sequences
testThinkFilter :: IO ()
testThinkFilter = do
  -- Test 1: Simple case - think block followed by content
  putStrLn "Test 1: Simple think then content"
  let tokens1 = ["<think>", "reasoning", "</think>", "The answer."]
      result1 = simulateStream tokens1
  putStrLn $ "  Tokens: " ++ show tokens1
  putStrLn $ "  Output: " ++ show result1
  putStrLn $ "  Expected: \"The answer.\""
  putStrLn $ "  " ++ if result1 == "The answer." then "PASS" else "FAIL"
  
  -- Test 2: Split tags across tokens
  putStrLn "\nTest 2: Split tags"
  let tokens2 = ["<thi", "nk>secret</thi", "nk>visible"]
      result2 = simulateStream tokens2
  putStrLn $ "  Tokens: " ++ show tokens2
  putStrLn $ "  Output: " ++ show result2
  putStrLn $ "  Expected: \"visible\""
  putStrLn $ "  " ++ if result2 == "visible" then "PASS" else "FAIL"
  
  -- Test 3: Content before think
  putStrLn "\nTest 3: Content before think"
  let tokens3 = ["Hello ", "<think>", "...", "</think>", " world"]
      result3 = simulateStream tokens3
  putStrLn $ "  Tokens: " ++ show tokens3
  putStrLn $ "  Output: " ++ show result3
  putStrLn $ "  Expected: \"Hello  world\""
  putStrLn $ "  " ++ if result3 == "Hello  world" then "PASS" else "FAIL"
  
  -- Test 4: No think block
  putStrLn "\nTest 4: No think block"
  let tokens4 = ["Just", " normal", " text"]
      result4 = simulateStream tokens4
  putStrLn $ "  Tokens: " ++ show tokens4
  putStrLn $ "  Output: " ++ show result4
  putStrLn $ "  Expected: \"Just normal text\""
  putStrLn $ "  " ++ if result4 == "Just normal text" then "PASS" else "FAIL"
  
  -- Test 5: Unclosed think (truncated)
  putStrLn "\nTest 5: Unclosed think block"
  let tokens5 = ["<think>", "reasoning forever..."]
      result5 = simulateStream tokens5
  putStrLn $ "  Tokens: " ++ show tokens5
  putStrLn $ "  Output: " ++ show result5
  putStrLn $ "  Expected: \"\""
  putStrLn $ "  " ++ if result5 == "" then "PASS" else "FAIL"

  -- Test 6: Partial tag that's not actually a tag
  putStrLn "\nTest 6: False partial (< but not <think>)"
  let tokens6 = ["x < y", " and ", "z > w"]
      result6 = simulateStream tokens6
  putStrLn $ "  Tokens: " ++ show tokens6
  putStrLn $ "  Output: " ++ show result6
  putStrLn $ "  Expected: \"x < y and z > w\""
  putStrLn $ "  " ++ if result6 == "x < y and z > w" then "PASS" else "FAIL"

-- | Simulate streaming tokens through the filter
simulateStream :: [Text] -> Text
simulateStream tokens = 
  let (finalFilter, outputs) = go initThinkFilter tokens []
      finalOutput = finalizeThinkFilter finalFilter
  in T.concat (reverse outputs) <> finalOutput
  where
    go tf [] acc = (tf, acc)
    go tf (t:ts) acc =
      let ThinkResult tf' out = feedThinkFilter tf t
      in go tf' ts (out : acc)
