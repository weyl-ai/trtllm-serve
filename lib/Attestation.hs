{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

-- | Attestation - Cryptographic identity and signed attestations
--
-- Provides agent continuity through:
--   - Ed25519 identity management
--   - Signed git commits for attestation chain
--   - Signature verification
--
-- This module supports the Aleph coeffect system's need for
-- provenance tracking and accountability.
--
-- @since 0.1.0

module Attestation
  ( -- * Types
    Identity(..)
  , Attestation(..)
  , AttestationType(..)
  , Coeffects(..)
  , SignatureStatus(..)
    -- * Identity operations
  , loadIdentity
  , getFingerprint
  , getDID
    -- * Attestation operations
  , createAttestation
  , getAttestationLog
  , verifyAttestation
    -- * Errors
  , AttestationError(..)
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show, iso8601ParseM)
import GHC.Generics
import System.Directory (doesFileExist, doesDirectoryExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)


-- ════════════════════════════════════════════════════════════════════════════════
-- Core Types
-- ════════════════════════════════════════════════════════════════════════════════

-- | Agent identity backed by Ed25519 keypair
data Identity = Identity
  { idFingerprint :: !Text     -- ^ SHA256 fingerprint
  , idDID         :: !Text     -- ^ did:key:... identifier
  , idPublicKey   :: !Text     -- ^ Base64 encoded public key
  , idKeyPath     :: !FilePath -- ^ Path to private key (for signing)
  } deriving (Show, Eq, Generic)

instance ToJSON Identity where
  toJSON Identity{..} = object
    [ "fingerprint" .= idFingerprint
    , "did"         .= idDID
    , "public_key"  .= idPublicKey
    ]

-- | Type of attestation
data AttestationType
  = AttestTask       -- ^ Completed a task
  | AttestReasoning  -- ^ Recorded reasoning
  | AttestDecision   -- ^ Made a decision
  | AttestCoeffect   -- ^ Discharged a coeffect
  | AttestCustom Text
  deriving (Show, Eq, Generic)

instance ToJSON AttestationType where
  toJSON = \case
    AttestTask        -> "task"
    AttestReasoning   -> "reasoning"
    AttestDecision    -> "decision"
    AttestCoeffect    -> "coeffect"
    AttestCustom t    -> toJSON t

instance FromJSON AttestationType where
  parseJSON = withText "AttestationType" $ \t -> pure $ case t of
    "task"      -> AttestTask
    "reasoning" -> AttestReasoning
    "decision"  -> AttestDecision
    "coeffect"  -> AttestCoeffect
    custom      -> AttestCustom custom

-- | Resources accessed during an operation (coeffects)
data Coeffects = Coeffects
  { coFilesystem :: !(Maybe Text)  -- ^ e.g., "read", "write", "readWrite"
  , coNetwork    :: !(Maybe Text)  -- ^ e.g., "read", "write", "readWrite"
  , coGpu        :: !(Maybe Text)  -- ^ e.g., "read" (inference), "write" (training)
  , coCustom     :: ![(Text, Text)]
  } deriving (Show, Eq, Generic)

instance ToJSON Coeffects where
  toJSON Coeffects{..} = object $ catMaybes
    [ ("filesystem" .=) <$> coFilesystem
    , ("network" .=) <$> coNetwork
    , ("gpu" .=) <$> coGpu
    ] ++ [k .= v | (k, v) <- coCustom]

instance FromJSON Coeffects where
  parseJSON = withObject "Coeffects" $ \v -> Coeffects
    <$> v .:? "filesystem"
    <*> v .:? "network"
    <*> v .:? "gpu"
    <*> pure []  -- Custom fields need explicit handling

emptyCoeffects :: Coeffects
emptyCoeffects = Coeffects Nothing Nothing Nothing []

-- | A signed attestation
data Attestation = Attestation
  { atCommitHash :: !Text
  , atTimestamp  :: !UTCTime
  , atType       :: !AttestationType
  , atContext    :: !Text
  , atThought    :: !(Maybe Text)
  , atAction     :: !(Maybe Text)
  , atCoeffects  :: !Coeffects
  , atSignature  :: !SignatureStatus
  } deriving (Show, Eq, Generic)

instance ToJSON Attestation where
  toJSON Attestation{..} = object $ catMaybes
    [ Just $ "commit"    .= atCommitHash
    , Just $ "timestamp" .= iso8601Show atTimestamp
    , Just $ "type"      .= atType
    , Just $ "context"   .= atContext
    , ("thought" .=) <$> atThought
    , ("action" .=) <$> atAction
    , Just $ "coeffects" .= atCoeffects
    , Just $ "signature" .= atSignature
    ]

-- | Signature verification status
data SignatureStatus
  = SigGood Text   -- ^ Valid signature, signer fingerprint
  | SigBad Text    -- ^ Invalid signature, reason
  | SigUnknown     -- ^ Signature not verified
  | SigNone        -- ^ No signature present
  deriving (Show, Eq, Generic)

instance ToJSON SignatureStatus where
  toJSON = \case
    SigGood fp  -> object ["status" .= ("good" :: Text), "signer" .= fp]
    SigBad r    -> object ["status" .= ("bad" :: Text), "reason" .= r]
    SigUnknown  -> object ["status" .= ("unknown" :: Text)]
    SigNone     -> object ["status" .= ("none" :: Text)]

-- | Errors that can occur during attestation operations
data AttestationError
  = IdentityNotFound FilePath
  | AttestationRepoNotFound FilePath
  | GitError Text
  | SigningError Text
  | ParseError Text
  deriving (Show, Eq, Generic)

instance ToJSON AttestationError where
  toJSON = \case
    IdentityNotFound p      -> object ["error" .= ("identity_not_found" :: Text), "path" .= p]
    AttestationRepoNotFound p -> object ["error" .= ("repo_not_found" :: Text), "path" .= p]
    GitError t              -> object ["error" .= ("git_error" :: Text), "message" .= t]
    SigningError t          -> object ["error" .= ("signing_error" :: Text), "message" .= t]
    ParseError t            -> object ["error" .= ("parse_error" :: Text), "message" .= t]


-- ════════════════════════════════════════════════════════════════════════════════
-- Configuration
-- ════════════════════════════════════════════════════════════════════════════════

-- | Default paths for identity and attestations
defaultIdentityDir :: FilePath
defaultIdentityDir = "/home/b7r6/.config/opencode/identity"

defaultAttestationDir :: FilePath
defaultAttestationDir = "/home/b7r6/.config/opencode/attestations"


-- ════════════════════════════════════════════════════════════════════════════════
-- Identity Operations
-- ════════════════════════════════════════════════════════════════════════════════

-- | Load identity from disk
loadIdentity :: Maybe FilePath -> IO (Either AttestationError Identity)
loadIdentity mDir = do
  let dir = fromMaybe defaultIdentityDir mDir
      pubPath = dir </> "claude.pub"
      privPath = "/run/agenix/claude-identity-key"  -- Decrypted at runtime
  
  pubExists <- doesFileExist pubPath
  if not pubExists
    then pure $ Left $ IdentityNotFound pubPath
    else do
      pubKey <- TIO.readFile pubPath
      
      -- Get fingerprint via ssh-keygen
      (exitCode, stdout, stderr) <- readProcessWithExitCode 
        "ssh-keygen" ["-lf", pubPath] ""
      
      case exitCode of
        ExitFailure _ -> pure $ Left $ ParseError $ T.pack stderr
        ExitSuccess -> do
          let fingerprint = extractFingerprint $ T.pack stdout
              did = computeDID pubKey
          pure $ Right $ Identity
            { idFingerprint = fingerprint
            , idDID = did
            , idPublicKey = T.strip pubKey
            , idKeyPath = privPath
            }

-- | Extract SHA256 fingerprint from ssh-keygen output
extractFingerprint :: Text -> Text
extractFingerprint output = 
  case T.words output of
    (_bits : fp : _rest) -> fp
    _ -> "unknown"

-- | Compute DID from public key (simplified)
computeDID :: Text -> Text
computeDID pubKey = "did:key:" <> T.take 52 (T.filter (/= ' ') pubKey)

-- | Get fingerprint for an identity
getFingerprint :: Identity -> Text
getFingerprint = idFingerprint

-- | Get DID for an identity
getDID :: Identity -> Text
getDID = idDID


-- ════════════════════════════════════════════════════════════════════════════════
-- Attestation Operations
-- ════════════════════════════════════════════════════════════════════════════════

-- | Create a new signed attestation
createAttestation 
  :: Identity 
  -> Maybe FilePath          -- ^ Attestation repo path
  -> AttestationType 
  -> Text                    -- ^ Context
  -> Maybe Text              -- ^ Thought
  -> Maybe Text              -- ^ Action
  -> Coeffects               -- ^ Resources used
  -> IO (Either AttestationError Attestation)
createAttestation identity mRepoPath attType context mThought mAction coeffects = do
  let repoPath = fromMaybe defaultAttestationDir mRepoPath
  
  repoExists <- doesDirectoryExist (repoPath </> ".git")
  if not repoExists
    then pure $ Left $ AttestationRepoNotFound repoPath
    else do
      -- Build commit message
      let typeStr = case attType of
            AttestTask       -> "task"
            AttestReasoning  -> "reasoning"
            AttestDecision   -> "decision"
            AttestCoeffect   -> "coeffect"
            AttestCustom t   -> T.unpack t
          
          message = unlines $ catMaybes
            [ Just $ "[attest:" ++ typeStr ++ "] " ++ T.unpack (T.take 50 context)
            , Just ""
            , Just $ "CONTEXT: " ++ T.unpack context
            , ("THOUGHT: " ++) . T.unpack <$> mThought
            , ("ACTION: " ++) . T.unpack <$> mAction
            , Just ""
            , Just "COEFFECTS:"
            , ("- filesystem: " ++) . T.unpack <$> coFilesystem coeffects
            , ("- network: " ++) . T.unpack <$> coNetwork coeffects
            , ("- gpu: " ++) . T.unpack <$> coGpu coeffects
            ]
      
      -- Create signed commit
      (exitCode, stdout, stderr) <- readProcessWithExitCode 
        "git" 
        [ "-C", repoPath
        , "commit"
        , "--allow-empty"
        , "-S"
        , "-m", message
        ] ""
      
      case exitCode of
        ExitFailure _ -> pure $ Left $ GitError $ T.pack stderr
        ExitSuccess -> do
          -- Get the commit hash
          (_, hashOut, _) <- readProcessWithExitCode
            "git" ["-C", repoPath, "rev-parse", "HEAD"] ""
          
          let commitHash = T.strip $ T.pack hashOut
          
          -- Get timestamp
          (_, dateOut, _) <- readProcessWithExitCode
            "git" ["-C", repoPath, "log", "-1", "--format=%cI"] ""
          
          mTimestamp <- iso8601ParseM (T.unpack $ T.strip $ T.pack dateOut)
          case mTimestamp of
            Nothing -> pure $ Left $ ParseError "Could not parse commit timestamp"
            Just timestamp -> pure $ Right $ Attestation
              { atCommitHash = commitHash
              , atTimestamp = timestamp
              , atType = attType
              , atContext = context
              , atThought = mThought
              , atAction = mAction
              , atCoeffects = coeffects
              , atSignature = SigGood (idFingerprint identity)
              }

-- | Get attestation log (recent commits)
getAttestationLog 
  :: Maybe FilePath  -- ^ Attestation repo path
  -> Int             -- ^ Number of entries
  -> IO (Either AttestationError [Attestation])
getAttestationLog mRepoPath limit = do
  let repoPath = fromMaybe defaultAttestationDir mRepoPath
  
  repoExists <- doesDirectoryExist (repoPath </> ".git")
  if not repoExists
    then pure $ Left $ AttestationRepoNotFound repoPath
    else do
      -- Get log with signature status
      (exitCode, stdout, _) <- readProcessWithExitCode
        "git"
        [ "-C", repoPath
        , "log"
        , "--show-signature"
        , "--format=%H|%cI|%s|%G?|%GF"
        , "-n", show limit
        ] ""
      
      case exitCode of
        ExitFailure _ -> pure $ Left $ GitError "Failed to read git log"
        ExitSuccess -> do
          let entries = filter (not . T.null) $ T.lines $ T.pack stdout
          attestations <- mapM (parseLogEntry repoPath) entries
          pure $ Right $ catMaybes attestations

-- | Parse a single log entry
parseLogEntry :: FilePath -> Text -> IO (Maybe Attestation)
parseLogEntry repoPath line = do
  case T.splitOn "|" line of
    [hash, date, subject, sigStatus, sigFp] -> do
      mTimestamp <- iso8601ParseM (T.unpack date)
      case mTimestamp of
        Nothing -> pure Nothing
        Just ts -> do
          -- Parse attestation type from subject
          let attType = parseAttestationType subject
              sigStat = parseSigStatus sigStatus sigFp
          
          -- Get full message for context
          (_, msgOut, _) <- readProcessWithExitCode
            "git" ["-C", repoPath, "log", "-1", "--format=%b", T.unpack hash] ""
          
          let (ctx, thought, action, coeffs) = parseCommitBody (T.pack msgOut)
          
          pure $ Just $ Attestation
            { atCommitHash = hash
            , atTimestamp = ts
            , atType = attType
            , atContext = ctx
            , atThought = thought
            , atAction = action
            , atCoeffects = coeffs
            , atSignature = sigStat
            }
    _ -> pure Nothing

parseAttestationType :: Text -> AttestationType
parseAttestationType subject
  | "[attest:task]" `T.isInfixOf` subject      = AttestTask
  | "[attest:reasoning]" `T.isInfixOf` subject = AttestReasoning
  | "[attest:decision]" `T.isInfixOf` subject  = AttestDecision
  | "[attest:coeffect]" `T.isInfixOf` subject  = AttestCoeffect
  | otherwise = AttestCustom "unknown"

parseSigStatus :: Text -> Text -> SignatureStatus
parseSigStatus status fingerprint = case T.strip status of
  "G" -> SigGood fingerprint
  "B" -> SigBad "Bad signature"
  "U" -> SigGood fingerprint  -- Good sig, unknown key
  "X" -> SigBad "Signature expired"
  "Y" -> SigBad "Signature from expired key"
  "R" -> SigBad "Signature from revoked key"
  "E" -> SigBad "Cannot verify signature"
  "N" -> SigNone
  _   -> SigUnknown

parseCommitBody :: Text -> (Text, Maybe Text, Maybe Text, Coeffects)
parseCommitBody body = 
  let lns = T.lines body
      findField prefix = T.strip . T.drop (T.length prefix) <$> 
                         find (prefix `T.isPrefixOf`) lns
      ctx = fromMaybe "" $ findField "CONTEXT: "
      thought = findField "THOUGHT: "
      action = findField "ACTION: "
      
      -- Parse coeffects section
      coeffects = parseCoeffects lns
  in (ctx, thought, action, coeffects)
  where
    find p = listToMaybe . filter p
    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x

parseCoeffects :: [Text] -> Coeffects
parseCoeffects lns = Coeffects
  { coFilesystem = extractCoeffect "- filesystem: "
  , coNetwork    = extractCoeffect "- network: "
  , coGpu        = extractCoeffect "- gpu: "
  , coCustom     = []
  }
  where
    extractCoeffect prefix = 
      case filter (prefix `T.isPrefixOf`) lns of
        (x:_) -> Just $ T.strip $ T.drop (T.length prefix) x
        []    -> Nothing

-- | Verify an attestation signature
verifyAttestation :: Maybe FilePath -> Text -> IO (Either AttestationError SignatureStatus)
verifyAttestation mRepoPath commitHash = do
  let repoPath = fromMaybe defaultAttestationDir mRepoPath
  
  (exitCode, stdout, _) <- readProcessWithExitCode
    "git" ["-C", repoPath, "verify-commit", "--raw", T.unpack commitHash] ""
  
  case exitCode of
    ExitSuccess -> pure $ Right $ SigGood "verified"
    ExitFailure _ -> 
      if "GOODSIG" `T.isInfixOf` T.pack stdout
        then pure $ Right $ SigGood "verified"
        else pure $ Right $ SigBad "Signature verification failed"
