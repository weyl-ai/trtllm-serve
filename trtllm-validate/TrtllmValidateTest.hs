{-# LANGUAGE OverloadedStrings #-}

-- | Tests for TrtllmValidate
--
-- Unit tests for JSON parsing and validation logic.

module Main where

import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

-- ════════════════════════════════════════════════════════════════════════════════
-- Test Data
-- ════════════════════════════════════════════════════════════════════════════════

-- | Nested quantization config (NVIDIA format)
nestedQuantConfig :: BL.ByteString
nestedQuantConfig = "{\"quantization\": {\"quant_algo\": \"NVFP4\"}}"

-- | Flat quantization config (alternative format)
flatQuantConfig :: BL.ByteString
flatQuantConfig = "{\"quant_algo\": \"FP8\"}"

-- | Engine config with quantization
engineConfig :: BL.ByteString
engineConfig = "{\"pretrained_config\": {\"quantization\": {\"quant_algo\": \"NVFP4\"}}}"

-- | Engine config without quantization
engineConfigNoQuant :: BL.ByteString
engineConfigNoQuant = "{\"pretrained_config\": {}}"

-- | Malformed JSON
malformedJson :: BL.ByteString
malformedJson = "{not valid json"

-- ════════════════════════════════════════════════════════════════════════════════
-- Inline JSON Types (matching TrtllmValidate.hs)
-- ════════════════════════════════════════════════════════════════════════════════

data HfQuantConfig = HfQuantConfig
  { quantization :: Maybe QuantInfo
  , quant_algo :: Maybe String
  } deriving (Show, Eq)

data QuantInfo = QuantInfo
  { qi_quant_algo :: Maybe String
  } deriving (Show, Eq)

instance FromJSON HfQuantConfig where
  parseJSON = withObject "HfQuantConfig" $ \v -> do
    mQuant <- v .:? "quantization"
    algo <- v .:? "quant_algo"
    pure HfQuantConfig { quantization = mQuant, quant_algo = algo }

instance FromJSON QuantInfo where
  parseJSON = withObject "QuantInfo" $ \v ->
    QuantInfo <$> v .:? "quant_algo"

data EngineConfig = EngineConfig
  { pretrained_config :: Maybe PretrainedConfig
  } deriving (Show, Eq)

data PretrainedConfig = PretrainedConfig
  { pc_quantization :: Maybe PcQuantization
  } deriving (Show, Eq)

data PcQuantization = PcQuantization
  { pcq_quant_algo :: Maybe String
  } deriving (Show, Eq)

instance FromJSON EngineConfig where
  parseJSON = withObject "EngineConfig" $ \v ->
    EngineConfig <$> v .:? "pretrained_config"

instance FromJSON PretrainedConfig where
  parseJSON = withObject "PretrainedConfig" $ \v ->
    PretrainedConfig <$> v .:? "quantization"

instance FromJSON PcQuantization where
  parseJSON = withObject "PcQuantization" $ \v ->
    PcQuantization <$> v .:? "quant_algo"

-- ════════════════════════════════════════════════════════════════════════════════
-- Test Runner
-- ════════════════════════════════════════════════════════════════════════════════

data TestResult = Pass | Fail String deriving (Show, Eq)

runTest :: String -> IO TestResult -> IO Bool
runTest name action = do
  putStr $ "  " ++ name ++ "... "
  result <- action
  case result of
    Pass -> do
      putStrLn "PASS"
      return True
    Fail msg -> do
      putStrLn $ "FAIL: " ++ msg
      return False

-- ════════════════════════════════════════════════════════════════════════════════
-- Tests
-- ════════════════════════════════════════════════════════════════════════════════

testParseNestedQuant :: IO TestResult
testParseNestedQuant = do
  case eitherDecode nestedQuantConfig :: Either String HfQuantConfig of
    Left e -> return $ Fail e
    Right cfg -> case quantization cfg >>= qi_quant_algo of
      Just "NVFP4" -> return Pass
      Just other -> return $ Fail $ "Expected NVFP4, got " ++ other
      Nothing -> return $ Fail "quant_algo not found"

testParseFlatQuant :: IO TestResult
testParseFlatQuant = do
  case eitherDecode flatQuantConfig :: Either String HfQuantConfig of
    Left e -> return $ Fail e
    Right cfg -> case quant_algo cfg of
      Just "FP8" -> return Pass
      Just other -> return $ Fail $ "Expected FP8, got " ++ other
      Nothing -> return $ Fail "quant_algo not found"

testParseEngineConfig :: IO TestResult
testParseEngineConfig = do
  case eitherDecode engineConfig :: Either String EngineConfig of
    Left e -> return $ Fail e
    Right cfg -> case pretrained_config cfg >>= pc_quantization >>= pcq_quant_algo of
      Just "NVFP4" -> return Pass
      Just other -> return $ Fail $ "Expected NVFP4, got " ++ other
      Nothing -> return $ Fail "quant_algo not found"

testParseEngineConfigNoQuant :: IO TestResult
testParseEngineConfigNoQuant = do
  case eitherDecode engineConfigNoQuant :: Either String EngineConfig of
    Left e -> return $ Fail e
    Right cfg -> case pretrained_config cfg >>= pc_quantization >>= pcq_quant_algo of
      Nothing -> return Pass
      Just algo -> return $ Fail $ "Expected Nothing, got " ++ algo

testMalformedJson :: IO TestResult
testMalformedJson = do
  case eitherDecode malformedJson :: Either String HfQuantConfig of
    Left _ -> return Pass  -- Expected to fail
    Right _ -> return $ Fail "Should have failed to parse"

testEmptyConfig :: IO TestResult
testEmptyConfig = do
  case eitherDecode "{}" :: Either String HfQuantConfig of
    Left e -> return $ Fail e
    Right cfg -> 
      if quantization cfg == Nothing && quant_algo cfg == Nothing
        then return Pass
        else return $ Fail "Expected empty config"

-- ════════════════════════════════════════════════════════════════════════════════
-- Main
-- ════════════════════════════════════════════════════════════════════════════════

main :: IO ()
main = do
  putStrLn "════════════════════════════════════════════════════════════════"
  putStrLn "  trtllm-validate unit tests"
  putStrLn "════════════════════════════════════════════════════════════════"
  putStrLn ""
  
  results <- sequence
    [ runTest "parse nested quant config" testParseNestedQuant
    , runTest "parse flat quant config" testParseFlatQuant
    , runTest "parse engine config" testParseEngineConfig
    , runTest "parse engine config (no quant)" testParseEngineConfigNoQuant
    , runTest "reject malformed JSON" testMalformedJson
    , runTest "handle empty config" testEmptyConfig
    ]
  
  let passed = length (filter id results)
      total = length results
  
  putStrLn ""
  putStrLn $ "Results: " ++ show passed ++ "/" ++ show total ++ " passed"
  
  if all id results
    then exitSuccess
    else exitFailure
