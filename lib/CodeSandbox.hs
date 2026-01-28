{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Code Sandbox - Isolated workspaces for code editing and compilation
--
-- Supports the "Language Coset": Lean, Dhall, PureScript, Haskell, Rust
--
-- This module provides pure domain logic where possible, with IO operations
-- cleanly separated. Designed for reuse across different server frameworks.
--
-- @since 0.1.0

module CodeSandbox
  ( -- * Types
    WorkspaceId(..)
  , Workspace(..)
  , Language(..)
  , CompileResult(..)
  , AstMatch(..)
  , SandboxError(..)
    -- * Language detection
  , detectLanguage
  , languageExtensions
  , astGrepLanguage
    -- * Compile commands (pure)
  , compileCommand
  , CompileCommand(..)
    -- * State management
  , SandboxState
  , newSandboxState
  , createWorkspace
  , getWorkspace
  , listWorkspaces
    -- * File operations
  , writeFile
  , readFile
  , listFiles
    -- * AST operations
  , astSearch
  , astReplace
    -- * Compilation
  , compile
  ) where

import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Control.Monad (forM)
import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Generics
import System.Directory
import System.Exit (ExitCode(..))
import qualified System.FilePath as FP
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process (readProcessWithExitCode)
import Prelude hiding (readFile, writeFile)


-- ════════════════════════════════════════════════════════════════════════════════
-- Core Types
-- ════════════════════════════════════════════════════════════════════════════════

-- | Unique identifier for a workspace
newtype WorkspaceId = WorkspaceId { unWorkspaceId :: Text }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | A workspace is an isolated directory for code operations
data Workspace = Workspace
  { wsId   :: !WorkspaceId
  , wsPath :: !FilePath
  } deriving (Show, Eq, Generic)

instance ToJSON Workspace where
  toJSON Workspace{..} = object
    [ "id"   .= wsId
    , "path" .= wsPath
    ]

-- | Supported languages in the "Language Coset"
data Language
  = Rust
  | Haskell
  | Lean
  | Dhall
  | PureScript
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance ToJSON Language where
  toJSON = \case
    Rust       -> "rust"
    Haskell    -> "haskell"
    Lean       -> "lean"
    Dhall      -> "dhall"
    PureScript -> "purescript"

instance FromJSON Language where
  parseJSON = withText "Language" $ \t -> case T.toLower t of
    "rust"       -> pure Rust
    "haskell"    -> pure Haskell
    "lean"       -> pure Lean
    "dhall"      -> pure Dhall
    "purescript" -> pure PureScript
    "purs"       -> pure PureScript
    _            -> fail $ "Unknown language: " <> T.unpack t

-- | Result of a compilation attempt
data CompileResult = CompileResult
  { crSuccess  :: !Bool
  , crLanguage :: !Language
  , crStdout   :: !Text
  , crStderr   :: !Text
  , crExitCode :: !Int
  } deriving (Show, Eq, Generic)

instance ToJSON CompileResult where
  toJSON CompileResult{..} = object
    [ "success"   .= crSuccess
    , "language"  .= crLanguage
    , "stdout"    .= crStdout
    , "stderr"    .= crStderr
    , "exit_code" .= crExitCode
    ]

-- | A match from AST pattern search
data AstMatch = AstMatch
  { amFile       :: !Text
  , amLine       :: !Int
  , amColumn     :: !Int
  , amText       :: !Text
  , amReplacement :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON AstMatch where
  toJSON AstMatch{..} = object $
    [ "file"   .= amFile
    , "line"   .= amLine
    , "column" .= amColumn
    , "text"   .= amText
    ] ++ maybe [] (\r -> ["replacement" .= r]) amReplacement

instance FromJSON AstMatch where
  parseJSON = withObject "AstMatch" $ \v -> do
    file <- v .: "file"
    -- ast-grep returns range.start.line/column
    mRange <- v .:? "range"
    let (line, col) = case mRange of
          Just r  -> extractPosition r
          Nothing -> (0, 0)
    text <- v .: "text"
    repl <- v .:? "replacement"
    pure $ AstMatch file line col text repl
    where
      extractPosition :: Value -> (Int, Int)
      extractPosition (Object r) = 
        case (lookup' "start" r >>= lookup' "line", lookup' "start" r >>= lookup' "column") of
          (Just l, Just c) -> (l, c)
          _ -> (0, 0)
      extractPosition _ = (0, 0)
      
      lookup' :: FromJSON a => Key -> Object -> Maybe a
      lookup' k o = parseMaybe (.: k) o

-- | Errors that can occur during sandbox operations
data SandboxError
  = FileNotFound FilePath
  | WorkspaceNotFound WorkspaceId
  | UnknownLanguage FilePath
  | AstGrepError Text
  | CompileError Text
  | IOError Text
  deriving (Show, Eq, Generic)

instance ToJSON SandboxError where
  toJSON = \case
    FileNotFound p      -> object ["error" .= ("file_not_found" :: Text), "path" .= p]
    WorkspaceNotFound w -> object ["error" .= ("workspace_not_found" :: Text), "id" .= w]
    UnknownLanguage p   -> object ["error" .= ("unknown_language" :: Text), "path" .= p]
    AstGrepError t      -> object ["error" .= ("ast_grep_error" :: Text), "message" .= t]
    CompileError t      -> object ["error" .= ("compile_error" :: Text), "message" .= t]
    IOError t           -> object ["error" .= ("io_error" :: Text), "message" .= t]


-- ════════════════════════════════════════════════════════════════════════════════
-- Pure Functions
-- ════════════════════════════════════════════════════════════════════════════════

-- | Detect language from file extension
detectLanguage :: FilePath -> Maybe Language
detectLanguage path = case FP.takeExtension path of
  ".rs"    -> Just Rust
  ".hs"    -> Just Haskell
  ".lhs"   -> Just Haskell
  ".lean"  -> Just Lean
  ".dhall" -> Just Dhall
  ".purs"  -> Just PureScript
  _        -> Nothing

-- | Get file extensions for a language
languageExtensions :: Language -> [String]
languageExtensions = \case
  Rust       -> [".rs"]
  Haskell    -> [".hs", ".lhs"]
  Lean       -> [".lean"]
  Dhall      -> [".dhall"]
  PureScript -> [".purs"]

-- | Get ast-grep language identifier
astGrepLanguage :: Language -> String
astGrepLanguage = \case
  Rust       -> "rust"
  Haskell    -> "haskell"
  Lean       -> "lean"
  Dhall      -> "dhall"  -- May need custom tree-sitter grammar
  PureScript -> "purescript"

-- | Command specification for compilation
data CompileCommand = CompileCommand
  { ccProgram :: !String
  , ccArgs    :: ![String]
  , ccWorkDir :: !FilePath
  } deriving (Show, Eq, Generic)

instance ToJSON CompileCommand

-- | Generate compile command for a given language (pure)
compileCommand :: Language -> FilePath -> FilePath -> CompileCommand
compileCommand lang wsPath filePath = CompileCommand prog args wsPath
  where
    outDir  = wsPath FP.</> ".sandbox-out"
    outPath = outDir FP.</> FP.takeBaseName filePath
    
    (prog, args) = case lang of
      Rust       -> ("rustc", ["--edition=2021", "--emit=metadata", "-o", outPath, filePath])
      Haskell    -> ("ghc", ["-fno-code", "-fwrite-interface", "-outputdir", outDir, filePath])
      Lean       -> ("lean", ["--run", filePath])
      Dhall      -> ("dhall", ["type", "--file", filePath])
      PureScript -> ("purs", ["compile", filePath, "--output", outDir])


-- ════════════════════════════════════════════════════════════════════════════════
-- State Management
-- ════════════════════════════════════════════════════════════════════════════════

-- | Thread-safe state for managing workspaces
data SandboxState = SandboxState
  { ssWorkspaces :: TVar (Map WorkspaceId Workspace)
  }

-- | Create a new sandbox state
newSandboxState :: IO SandboxState
newSandboxState = SandboxState <$> newTVarIO Map.empty

-- | Create a new workspace or return existing one
createWorkspace :: SandboxState -> Maybe WorkspaceId -> IO Workspace
createWorkspace SandboxState{..} mWsId = do
  wsId <- case mWsId of
    Just wid -> pure wid
    Nothing  -> do
      now <- getCurrentTime
      let ts = show (round (utcTimeToPOSIXSeconds now) :: Int)
      pure $ WorkspaceId $ T.pack $ take 12 ts
  
  -- Check if already exists
  existing <- atomically $ Map.lookup wsId <$> readTVar ssWorkspaces
  case existing of
    Just ws -> pure ws
    Nothing -> do
      tmpBase <- getCanonicalTemporaryDirectory
      wsPath <- createTempDirectory tmpBase ("sandbox-" ++ T.unpack (unWorkspaceId wsId) ++ "-")
      let ws = Workspace wsId wsPath
      atomically $ modifyTVar' ssWorkspaces (Map.insert wsId ws)
      pure ws

-- | Get an existing workspace
getWorkspace :: SandboxState -> WorkspaceId -> IO (Maybe Workspace)
getWorkspace SandboxState{..} wsId = 
  atomically $ Map.lookup wsId <$> readTVar ssWorkspaces

-- | List all workspaces
listWorkspaces :: SandboxState -> IO [Workspace]
listWorkspaces SandboxState{..} =
  atomically $ Map.elems <$> readTVar ssWorkspaces


-- ════════════════════════════════════════════════════════════════════════════════
-- File Operations
-- ════════════════════════════════════════════════════════════════════════════════

-- | Write content to a file in a workspace
writeFile :: Workspace -> FilePath -> Text -> IO (Either SandboxError ())
writeFile Workspace{..} relPath content = do
  let fullPath = wsPath FP.</> relPath
      dir = FP.takeDirectory fullPath
  result <- try $ do
    createDirectoryIfMissing True dir
    TIO.writeFile fullPath content
  case result of
    Left (e :: SomeException) -> pure $ Left $ IOError $ T.pack $ show e
    Right ()                  -> pure $ Right ()

-- | Read content from a file in a workspace
readFile :: Workspace -> FilePath -> IO (Either SandboxError Text)
readFile Workspace{..} relPath = do
  let fullPath = wsPath FP.</> relPath
  exists <- doesFileExist fullPath
  if not exists
    then pure $ Left $ FileNotFound relPath
    else do
      result <- try $ TIO.readFile fullPath
      case result of
        Left (e :: SomeException) -> pure $ Left $ IOError $ T.pack $ show e
        Right content             -> pure $ Right content

-- | List all files in a workspace (recursively)
listFiles :: Workspace -> IO [FilePath]
listFiles Workspace{..} = do
  exists <- doesDirectoryExist wsPath
  if not exists
    then pure []
    else listFilesRecursive wsPath wsPath
  where
    listFilesRecursive root dir = do
      entries <- listDirectory dir
      fmap concat $ forM entries $ \entry -> do
        let path = dir FP.</> entry
        isDir <- doesDirectoryExist path
        if isDir && not (isHidden entry)
          then listFilesRecursive root path
          else if not (isHidden entry)
               then pure [FP.makeRelative root path]
               else pure []
    
    isHidden = ("." `isPrefixOf`)
    isPrefixOf p s = take (length p) s == p


-- ════════════════════════════════════════════════════════════════════════════════
-- AST Operations
-- ════════════════════════════════════════════════════════════════════════════════

-- | Search for AST pattern matches
astSearch :: Workspace -> Text -> Maybe Language -> IO (Either SandboxError [AstMatch])
astSearch Workspace{..} pattern mLang = do
  let langArg = maybe [] (\l -> ["-l", astGrepLanguage l]) mLang
      args = ["run", "-p", T.unpack pattern, "--json", wsPath] ++ langArg
  
  (exitCode, stdout, stderr) <- readProcessWithExitCode "ast-grep" args ""
  
  case exitCode of
    ExitFailure _ 
      | "error" `T.isInfixOf` T.pack stderr -> 
          pure $ Left $ AstGrepError $ T.pack stderr
    _ -> case eitherDecodeStrict' (TE.encodeUtf8 $ T.pack stdout) of
      Left e  -> pure $ Left $ AstGrepError $ "Parse error: " <> T.pack e
      Right matches -> pure $ Right matches

-- | Search and replace using AST patterns
astReplace :: Workspace -> Text -> Text -> Maybe Language -> IO (Either SandboxError ([AstMatch], Int))
astReplace Workspace{..} pattern replacement mLang = do
  let langArg = maybe [] (\l -> ["-l", astGrepLanguage l]) mLang
  
  -- First, get the matches (dry-run)
  let searchArgs = ["run", "-p", T.unpack pattern, "-r", T.unpack replacement, "--json", wsPath] ++ langArg
  (_, searchOut, _) <- readProcessWithExitCode "ast-grep" searchArgs ""
  
  let matches = case eitherDecodeStrict' (TE.encodeUtf8 $ T.pack searchOut) of
        Left _   -> []
        Right ms -> ms
  
  -- Then apply the replacement in-place
  let applyArgs = ["run", "-p", T.unpack pattern, "-r", T.unpack replacement, "-U", wsPath] ++ langArg
  (exitCode, _, stderr) <- readProcessWithExitCode "ast-grep" applyArgs ""
  
  case exitCode of
    ExitFailure _ -> pure $ Left $ AstGrepError $ T.pack stderr
    ExitSuccess   -> pure $ Right (matches, length $ map amFile matches)


-- ════════════════════════════════════════════════════════════════════════════════
-- Compilation
-- ════════════════════════════════════════════════════════════════════════════════

-- | Compile a file and return the result
compile :: Workspace -> FilePath -> IO (Either SandboxError CompileResult)
compile ws@Workspace{..} relPath = do
  let fullPath = wsPath FP.</> relPath
  exists <- doesFileExist fullPath
  if not exists
    then pure $ Left $ FileNotFound relPath
    else case detectLanguage relPath of
      Nothing -> pure $ Left $ UnknownLanguage relPath
      Just lang -> do
        let CompileCommand{..} = compileCommand lang wsPath fullPath
        
        -- Ensure output directory exists
        let outDir = wsPath FP.</> ".sandbox-out"
        createDirectoryIfMissing True outDir
        
        (exitCode, stdout, stderr) <- readProcessWithExitCode ccProgram ccArgs ccWorkDir
        
        let exitInt = case exitCode of
              ExitSuccess   -> 0
              ExitFailure n -> n
        
        pure $ Right $ CompileResult
          { crSuccess  = exitCode == ExitSuccess
          , crLanguage = lang
          , crStdout   = T.pack stdout
          , crStderr   = T.pack stderr
          , crExitCode = exitInt
          }
