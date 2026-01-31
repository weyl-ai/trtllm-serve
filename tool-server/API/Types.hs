{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | API Types for Tool Server with OpenAPI3 instances
--
-- All request/response types use generic derivation with a field label
-- modifier that strips the type prefix and converts to snake_case.
-- This ensures ToJSON, FromJSON, and ToSchema all agree on field names.
--
-- Field naming convention in Haskell: prefix with abbreviation of type name
-- (e.g., wfrWorkspaceId for WriteFileReq) to avoid record field conflicts.
-- JSON/OpenAPI uses snake_case without prefix (e.g., workspace_id).
--
-- @since 0.1.0

module API.Types
  ( -- * Workspace API
    CreateWorkspaceReq(..)
  , WorkspaceResp(..)
  , ListWorkspacesResp(..)
    -- * File API
  , WriteFileReq(..)
  , ReadFileReq(..)
  , FileContentResp(..)
  , ListFilesResp(..)
    -- * AST API
  , AstSearchReq(..)
  , AstReplaceReq(..)
  , AstMatchResp(..)
  , AstSearchResp(..)
  , AstReplaceResp(..)
    -- * Compile API
  , CompileReq(..)
  , CompileResp(..)
    -- * Identity API
  , IdentityResp(..)
    -- * Attestation API
  , CreateAttestationReq(..)
  , CoeffectsReq(..)
  , SignatureResp(..)
  , AttestationResp(..)
  , AttestationLogResp(..)
    -- * Search API (tools)
  , SearchResp(..)
  , SearchResult(..)
  , ReadUrlResp(..)
    -- * CAS API
  , DigestResp(..)
  , PutBlobReq(..)
  , PutBlobResp(..)
  , GetBlobResp(..)
  , CASInfoResp(..)

    -- * Common
  , ErrorResp(..)
  , SuccessResp(..)
  ) where

import Data.Aeson
import Data.Char (isUpper, toLower)
import Data.OpenApi
import Data.Text (Text)
import GHC.Generics

-- ════════════════════════════════════════════════════════════════════════════════
-- JSON/Schema Options
-- ════════════════════════════════════════════════════════════════════════════════

-- | Strip type prefix from field name and convert to snake_case
--
-- Examples:
--   wfrWorkspaceId -> workspace_id
--   carAttestType  -> attest_type
--   srStatus       -> status
stripPrefixSnakeCase :: String -> String
stripPrefixSnakeCase = camelToSnake . dropWhile (not . isUpper)
  where
    camelToSnake :: String -> String
    camelToSnake [] = []
    camelToSnake (x:xs) = toLower x : go xs

    go :: String -> String
    go [] = []
    go (x:xs)
      | isUpper x = '_' : toLower x : go xs
      | otherwise = x : go xs

-- | Aeson options using our field label modifier
jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier = stripPrefixSnakeCase }

-- | OpenAPI schema options matching our Aeson options
schemaOptions :: SchemaOptions
schemaOptions = fromAesonOptions jsonOptions


-- ════════════════════════════════════════════════════════════════════════════════
-- Common Types
-- ════════════════════════════════════════════════════════════════════════════════

data ErrorResp = ErrorResp
  { errError   :: !Text
  , errMessage :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON ErrorResp where
  toJSON = genericToJSON jsonOptions
instance FromJSON ErrorResp where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema ErrorResp where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

data SuccessResp = SuccessResp
  { srStatus :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON SuccessResp where
  toJSON = genericToJSON jsonOptions
instance FromJSON SuccessResp where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema SuccessResp where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions




-- ════════════════════════════════════════════════════════════════════════════════
-- Workspace API Types
-- ════════════════════════════════════════════════════════════════════════════════

data CreateWorkspaceReq = CreateWorkspaceReq
  { cwrWorkspaceId :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON CreateWorkspaceReq where
  toJSON = genericToJSON jsonOptions
instance FromJSON CreateWorkspaceReq where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema CreateWorkspaceReq where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

data WorkspaceResp = WorkspaceResp
  { wrId   :: !Text
  , wrPath :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON WorkspaceResp where
  toJSON = genericToJSON jsonOptions
instance FromJSON WorkspaceResp where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema WorkspaceResp where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

data ListWorkspacesResp = ListWorkspacesResp
  { lwrWorkspaces :: ![WorkspaceResp]
  } deriving (Show, Eq, Generic)

instance ToJSON ListWorkspacesResp where
  toJSON = genericToJSON jsonOptions
instance FromJSON ListWorkspacesResp where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema ListWorkspacesResp where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions


-- ════════════════════════════════════════════════════════════════════════════════
-- File API Types
-- ════════════════════════════════════════════════════════════════════════════════

data WriteFileReq = WriteFileReq
  { wfrWorkspaceId :: !Text
  , wfrPath        :: !Text
  , wfrContent     :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON WriteFileReq where
  toJSON = genericToJSON jsonOptions
instance FromJSON WriteFileReq where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema WriteFileReq where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

data ReadFileReq = ReadFileReq
  { rfrWorkspaceId :: !Text
  , rfrPath        :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON ReadFileReq where
  toJSON = genericToJSON jsonOptions
instance FromJSON ReadFileReq where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema ReadFileReq where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

data FileContentResp = FileContentResp
  { fcrPath    :: !Text
  , fcrContent :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON FileContentResp where
  toJSON = genericToJSON jsonOptions
instance FromJSON FileContentResp where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema FileContentResp where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

data ListFilesResp = ListFilesResp
  { lfrWorkspaceId :: !Text
  , lfrFiles       :: ![Text]
  } deriving (Show, Eq, Generic)

instance ToJSON ListFilesResp where
  toJSON = genericToJSON jsonOptions
instance FromJSON ListFilesResp where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema ListFilesResp where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions


-- ════════════════════════════════════════════════════════════════════════════════
-- AST API Types
-- ════════════════════════════════════════════════════════════════════════════════

data AstSearchReq = AstSearchReq
  { asrWorkspaceId :: !Text
  , asrPattern     :: !Text
  , asrLanguage    :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON AstSearchReq where
  toJSON = genericToJSON jsonOptions
instance FromJSON AstSearchReq where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema AstSearchReq where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

data AstReplaceReq = AstReplaceReq
  { arrWorkspaceId  :: !Text
  , arrPattern      :: !Text
  , arrReplacement  :: !Text
  , arrLanguage     :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON AstReplaceReq where
  toJSON = genericToJSON jsonOptions
instance FromJSON AstReplaceReq where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema AstReplaceReq where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

data AstMatchResp = AstMatchResp
  { amrFile        :: !Text
  , amrLine        :: !Int
  , amrColumn      :: !Int
  , amrText        :: !Text
  , amrReplacement :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON AstMatchResp where
  toJSON = genericToJSON jsonOptions
instance FromJSON AstMatchResp where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema AstMatchResp where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

data AstSearchResp = AstSearchResp
  { asrMatches :: ![AstMatchResp]
  , asrCount   :: !Int
  } deriving (Show, Eq, Generic)

instance ToJSON AstSearchResp where
  toJSON = genericToJSON jsonOptions
instance FromJSON AstSearchResp where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema AstSearchResp where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

data AstReplaceResp = AstReplaceResp
  { arrMatches       :: ![AstMatchResp]
  , arrFilesModified :: !Int
  } deriving (Show, Eq, Generic)

instance ToJSON AstReplaceResp where
  toJSON = genericToJSON jsonOptions
instance FromJSON AstReplaceResp where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema AstReplaceResp where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions


-- ════════════════════════════════════════════════════════════════════════════════
-- Compile API Types
-- ════════════════════════════════════════════════════════════════════════════════

data CompileReq = CompileReq
  { crWorkspaceId :: !Text
  , crPath        :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON CompileReq where
  toJSON = genericToJSON jsonOptions
instance FromJSON CompileReq where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema CompileReq where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

data CompileResp = CompileResp
  { cpSuccess   :: !Bool
  , cpLanguage  :: !Text
  , cpStdout    :: !Text
  , cpStderr    :: !Text
  , cpExitCode  :: !Int
  } deriving (Show, Eq, Generic)

instance ToJSON CompileResp where
  toJSON = genericToJSON jsonOptions
instance FromJSON CompileResp where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema CompileResp where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions


-- ════════════════════════════════════════════════════════════════════════════════
-- Identity API Types
-- ════════════════════════════════════════════════════════════════════════════════

data IdentityResp = IdentityResp
  { irFingerprint :: !Text
  , irDid         :: !Text
  , irPublicKey   :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON IdentityResp where
  toJSON = genericToJSON jsonOptions
instance FromJSON IdentityResp where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema IdentityResp where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions


-- ════════════════════════════════════════════════════════════════════════════════
-- Attestation API Types
-- ════════════════════════════════════════════════════════════════════════════════

data CoeffectsReq = CoeffectsReq
  { crqFilesystem :: !(Maybe Text)
  , crqNetwork    :: !(Maybe Text)
  , crqGpu        :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON CoeffectsReq where
  toJSON = genericToJSON jsonOptions
instance FromJSON CoeffectsReq where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema CoeffectsReq where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

data CreateAttestationReq = CreateAttestationReq
  { carAttestType :: !Text
  , carContext    :: !Text
  , carThought    :: !(Maybe Text)
  , carAction     :: !(Maybe Text)
  , carCoeffects  :: !(Maybe CoeffectsReq)
  } deriving (Show, Eq, Generic)

instance ToJSON CreateAttestationReq where
  toJSON = genericToJSON jsonOptions
instance FromJSON CreateAttestationReq where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema CreateAttestationReq where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

data SignatureResp = SignatureResp
  { sigStatus :: !Text
  , sigSigner :: !(Maybe Text)
  , sigReason :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON SignatureResp where
  toJSON = genericToJSON jsonOptions
instance FromJSON SignatureResp where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema SignatureResp where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

data AttestationResp = AttestationResp
  { atrCommit     :: !Text
  , atrTimestamp  :: !Text
  , atrAttestType :: !Text
  , atrContext    :: !Text
  , atrThought    :: !(Maybe Text)
  , atrAction     :: !(Maybe Text)
  , atrCoeffects  :: !CoeffectsReq
  , atrSignature  :: !SignatureResp
  } deriving (Show, Eq, Generic)

instance ToJSON AttestationResp where
  toJSON = genericToJSON jsonOptions
instance FromJSON AttestationResp where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema AttestationResp where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

data AttestationLogResp = AttestationLogResp
  { alrAttestations :: ![AttestationResp]
  , alrCount        :: !Int
  } deriving (Show, Eq, Generic)

instance ToJSON AttestationLogResp where
  toJSON = genericToJSON jsonOptions
instance FromJSON AttestationLogResp where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema AttestationLogResp where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions


-- ════════════════════════════════════════════════════════════════════════════════
-- Search/Tools API Types
-- ════════════════════════════════════════════════════════════════════════════════

data SearchResult = SearchResult
  { srtTitle   :: !Text
  , srtUrl     :: !Text
  , srtContent :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON SearchResult where
  toJSON = genericToJSON jsonOptions
instance FromJSON SearchResult where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema SearchResult where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

data SearchResp = SearchResp
  { srchStatus  :: !Text
  , srchQuery   :: !Text
  , srchResults :: ![SearchResult]
  , srchError   :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON SearchResp where
  toJSON = genericToJSON jsonOptions
instance FromJSON SearchResp where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema SearchResp where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

data ReadUrlResp = ReadUrlResp
  { rurStatus  :: !Text
  , rurUrl     :: !Text
  , rurTitle   :: !(Maybe Text)
  , rurContent :: !Text
  , rurError   :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON ReadUrlResp where
  toJSON = genericToJSON jsonOptions
instance FromJSON ReadUrlResp where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema ReadUrlResp where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions


-- ════════════════════════════════════════════════════════════════════════════════
-- CAS (Content-Addressable Storage) API Types
-- ════════════════════════════════════════════════════════════════════════════════

-- | Digest for content-addressed blobs
data DigestResp = DigestResp
  { drHash :: !Text  -- ^ SHA256 hex (64 chars)
  , drSize :: !Int   -- ^ Size in bytes
  } deriving (Show, Eq, Generic)

instance ToJSON DigestResp where
  toJSON = genericToJSON jsonOptions
instance FromJSON DigestResp where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema DigestResp where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | Request to upload a blob
data PutBlobReq = PutBlobReq
  { pbrContent :: !Text  -- ^ Base64-encoded content
  } deriving (Show, Eq, Generic)

instance ToJSON PutBlobReq where
  toJSON = genericToJSON jsonOptions
instance FromJSON PutBlobReq where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema PutBlobReq where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | Response from putting a blob
data PutBlobResp = PutBlobResp
  { pbrDigest :: !DigestResp
  , pbrStatus :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON PutBlobResp where
  toJSON = genericToJSON jsonOptions
instance FromJSON PutBlobResp where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema PutBlobResp where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | Response from getting a blob
data GetBlobResp = GetBlobResp
  { gbrContent :: !(Maybe Text)  -- ^ Base64-encoded content (Nothing if not found)
  , gbrDigest  :: !DigestResp
  , gbrStatus  :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON GetBlobResp where
  toJSON = genericToJSON jsonOptions
instance FromJSON GetBlobResp where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema GetBlobResp where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | CAS store info
data CASInfoResp = CASInfoResp
  { cirDataDir    :: !Text
  , cirBlobCount  :: !Int
  , cirNativelink :: !(Maybe Text)  -- ^ NativeLink endpoint if configured
  } deriving (Show, Eq, Generic)

instance ToJSON CASInfoResp where
  toJSON = genericToJSON jsonOptions
instance FromJSON CASInfoResp where
  parseJSON = genericParseJSON jsonOptions
instance ToSchema CASInfoResp where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions



