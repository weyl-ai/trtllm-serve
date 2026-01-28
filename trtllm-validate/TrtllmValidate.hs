{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | TensorRT-LLM Engine Validation Tool
--
-- Proper Haskell CLI for validating TRT-LLM engine builds.
-- Replaces shell heredocs with type-safe JSON parsing.
--
-- Commands:
--   detect-quant <path>     - Detect quantization from hf_quant_config.json
--   validate-engine <path> <expected-quant>  - Validate engine config.json
--   check-size <path> <quant> - Check engine size is reasonable

module Main where

import Control.Applicative ((<|>))
import Control.Monad (when, unless)
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist, getFileSize)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

-- | Quantization config as found in hf_quant_config.json
data HfQuantConfig = HfQuantConfig
  { quantization :: Maybe QuantInfo
  , quant_algo :: Maybe Text
  } deriving (Show)

data QuantInfo = QuantInfo
  { qi_quant_algo :: Maybe Text
  } deriving (Show)

instance FromJSON HfQuantConfig where
  parseJSON = withObject "HfQuantConfig" $ \v -> do
    -- Try nested "quantization" first, then top-level
    mQuant <- v .:? "quantization"
    algo <- v .:? "quant_algo"
    pure HfQuantConfig
      { quantization = mQuant
      , quant_algo = algo
      }

instance FromJSON QuantInfo where
  parseJSON = withObject "QuantInfo" $ \v ->
    QuantInfo <$> v .:? "quant_algo"

-- | Engine config.json structure
data EngineConfig = EngineConfig
  { pretrained_config :: Maybe PretrainedConfig
  } deriving (Show)

data PretrainedConfig = PretrainedConfig
  { pc_quantization :: Maybe PcQuantization
  } deriving (Show)

data PcQuantization = PcQuantization
  { pcq_quant_algo :: Maybe Text
  } deriving (Show)

instance FromJSON EngineConfig where
  parseJSON = withObject "EngineConfig" $ \v ->
    EngineConfig <$> v .:? "pretrained_config"

instance FromJSON PretrainedConfig where
  parseJSON = withObject "PretrainedConfig" $ \v ->
    PretrainedConfig <$> v .:? "quantization"

instance FromJSON PcQuantization where
  parseJSON = withObject "PcQuantization" $ \v ->
    PcQuantization <$> v .:? "quant_algo"

-- | Extract quant_algo from hf_quant_config.json
detectQuant :: FilePath -> IO ()
detectQuant modelPath = do
  let configPath = modelPath </> "hf_quant_config.json"
  exists <- doesFileExist configPath
  unless exists $ do
    -- No config file, not an error - just no quantization detected
    exitSuccess
  
  content <- BL.readFile configPath
  case eitherDecode content of
    Left e -> do
      -- Log warning but don't fail - might be a different config format
      TIO.hPutStrLn stderr $ "Warning: Failed to parse hf_quant_config.json: " <> T.pack e
      exitSuccess
    Right cfg -> do
      let algo = extractHfQuant cfg
      case algo of
        Nothing -> exitSuccess
        Just a  -> TIO.putStrLn a >> exitSuccess

-- | Extract quant_algo from HfQuantConfig (handles both nested and flat)
extractHfQuant :: HfQuantConfig -> Maybe Text
extractHfQuant cfg =
  -- Try nested first: {"quantization": {"quant_algo": "..."}}
  (quantization cfg >>= qi_quant_algo)
  -- Then try flat: {"quant_algo": "..."}
  <|> quant_algo cfg

-- | Validate engine config.json matches expected quantization
validateEngine :: FilePath -> Text -> IO ()
validateEngine enginePath expectedQuant = do
  let configPath = enginePath </> "config.json"
  exists <- doesFileExist configPath
  unless exists $ do
    err "config.json not found in engine directory"
    exitFailure
  
  content <- BL.readFile configPath
  case eitherDecode content of
    Left e -> do
      err $ "Failed to parse config.json: " <> T.pack e
      exitFailure
    Right cfg -> do
      let actual = extractEngineQuant cfg
      case actual of
        Nothing -> do
          err "quant_algo not found in engine config"
          err $ "Expected: " <> expectedQuant
          exitFailure
        Just a
          | a == expectedQuant -> do
              TIO.putStrLn $ "Quantization: " <> a
              exitSuccess
          | otherwise -> do
              TIO.hPutStrLn stderr "════════════════════════════════════════════════════════════════"
              TIO.hPutStrLn stderr "ERROR: Quantization mismatch!"
              TIO.hPutStrLn stderr $ "  Expected: " <> expectedQuant
              TIO.hPutStrLn stderr $ "  Found in config.json: " <> a
              TIO.hPutStrLn stderr ""
              TIO.hPutStrLn stderr "This usually means:"
              TIO.hPutStrLn stderr "  1. from_hugging_face() was called without quant_config"
              TIO.hPutStrLn stderr "  2. The model's hf_quant_config.json was not detected"
              TIO.hPutStrLn stderr ""
              TIO.hPutStrLn stderr "See: docs/archive/NVFP4-ENGINE-FAILURE-ANALYSIS.md"
              TIO.hPutStrLn stderr "════════════════════════════════════════════════════════════════"
              exitFailure

extractEngineQuant :: EngineConfig -> Maybe Text
extractEngineQuant cfg =
  pretrained_config cfg >>= pc_quantization >>= pcq_quant_algo

-- | Check engine size is reasonable for quantization level
checkSize :: FilePath -> Text -> IO ()
checkSize enginePath quant = do
  let rank0Path = enginePath </> "rank0.engine"
  exists <- doesFileExist rank0Path
  unless exists $ do
    err "rank0.engine not found"
    exitFailure
  
  sizeBytes <- getFileSize rank0Path
  let sizeGB = sizeBytes `div` (1024 * 1024 * 1024)
  
  TIO.putStrLn $ "Engine size: rank0.engine = " <> T.pack (show sizeGB) <> "GB"
  
  -- Size validation for NVFP4
  when (quant == "NVFP4" && sizeGB > 12) $ do
    TIO.hPutStrLn stderr "════════════════════════════════════════════════════════════════"
    TIO.hPutStrLn stderr $ "WARNING: Engine size (" <> T.pack (show sizeGB) <> "GB) seems too large for NVFP4"
    TIO.hPutStrLn stderr "  Expected: 5-8GB per rank for 32B model"
    TIO.hPutStrLn stderr "  This may indicate quantization was not applied correctly"
    TIO.hPutStrLn stderr "════════════════════════════════════════════════════════════════"
    -- Don't fail, just warn
  
  exitSuccess

err :: Text -> IO ()
err = TIO.hPutStrLn stderr

usage :: IO ()
usage = do
  hPutStrLn stderr "trtllm-validate - TensorRT-LLM Engine Validation"
  hPutStrLn stderr ""
  hPutStrLn stderr "Usage:"
  hPutStrLn stderr "  trtllm-validate detect-quant <model-path>"
  hPutStrLn stderr "  trtllm-validate validate-engine <engine-path> <expected-quant>"
  hPutStrLn stderr "  trtllm-validate check-size <engine-path> <quant>"
  exitFailure

main :: IO ()
main = getArgs >>= \case
  ["detect-quant", path] -> detectQuant path
  ["validate-engine", path, quant] -> validateEngine path (T.pack quant)
  ["check-size", path, quant] -> checkSize path (T.pack quant)
  _ -> usage
