{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | API Types for Tool Server with OpenAPI3 instances
--
-- All request/response types with proper schema derivation for
-- automatic OpenAPI spec generation.
--
-- Field naming convention: prefix with abbreviation of type name
-- to avoid record field conflicts.
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
import Data.OpenApi
import Data.Text (Text)
import GHC.Generics


-- ════════════════════════════════════════════════════════════════════════════════
-- Common Types
-- ════════════════════════════════════════════════════════════════════════════════

data ErrorResp = ErrorResp
  { errError   :: !Text
  , errMessage :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON ErrorResp where
  toJSON ErrorResp{..} = object
    [ "error" .= errError
    , "message" .= errMessage
    ]
instance FromJSON ErrorResp
instance ToSchema ErrorResp

data SuccessResp = SuccessResp
  { srStatus :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON SuccessResp where
  toJSON SuccessResp{..} = object ["status" .= srStatus]
instance FromJSON SuccessResp
instance ToSchema SuccessResp


-- ════════════════════════════════════════════════════════════════════════════════
-- Workspace API Types
-- ════════════════════════════════════════════════════════════════════════════════

data CreateWorkspaceReq = CreateWorkspaceReq
  { cwrWorkspaceId :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON CreateWorkspaceReq where
  toJSON CreateWorkspaceReq{..} = object ["workspace_id" .= cwrWorkspaceId]
instance FromJSON CreateWorkspaceReq where
  parseJSON = withObject "CreateWorkspaceReq" $ \v ->
    CreateWorkspaceReq <$> v .:? "workspace_id"
instance ToSchema CreateWorkspaceReq

data WorkspaceResp = WorkspaceResp
  { wrId   :: !Text
  , wrPath :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON WorkspaceResp where
  toJSON WorkspaceResp{..} = object ["id" .= wrId, "path" .= wrPath]
instance FromJSON WorkspaceResp
instance ToSchema WorkspaceResp

data ListWorkspacesResp = ListWorkspacesResp
  { lwrWorkspaces :: ![WorkspaceResp]
  } deriving (Show, Eq, Generic)

instance ToJSON ListWorkspacesResp where
  toJSON ListWorkspacesResp{..} = object ["workspaces" .= lwrWorkspaces]
instance FromJSON ListWorkspacesResp
instance ToSchema ListWorkspacesResp


-- ════════════════════════════════════════════════════════════════════════════════
-- File API Types
-- ════════════════════════════════════════════════════════════════════════════════

data WriteFileReq = WriteFileReq
  { wfrWorkspaceId :: !Text
  , wfrPath        :: !Text
  , wfrContent     :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON WriteFileReq where
  toJSON WriteFileReq{..} = object
    [ "workspace_id" .= wfrWorkspaceId
    , "path" .= wfrPath
    , "content" .= wfrContent
    ]
instance FromJSON WriteFileReq where
  parseJSON = withObject "WriteFileReq" $ \v -> WriteFileReq
    <$> v .: "workspace_id"
    <*> v .: "path"
    <*> v .: "content"
instance ToSchema WriteFileReq

data ReadFileReq = ReadFileReq
  { rfrWorkspaceId :: !Text
  , rfrPath        :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON ReadFileReq where
  toJSON ReadFileReq{..} = object
    [ "workspace_id" .= rfrWorkspaceId
    , "path" .= rfrPath
    ]
instance FromJSON ReadFileReq where
  parseJSON = withObject "ReadFileReq" $ \v -> ReadFileReq
    <$> v .: "workspace_id"
    <*> v .: "path"
instance ToSchema ReadFileReq

data FileContentResp = FileContentResp
  { fcrPath    :: !Text
  , fcrContent :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON FileContentResp where
  toJSON FileContentResp{..} = object ["path" .= fcrPath, "content" .= fcrContent]
instance FromJSON FileContentResp
instance ToSchema FileContentResp

data ListFilesResp = ListFilesResp
  { lfrWorkspaceId :: !Text
  , lfrFiles       :: ![Text]
  } deriving (Show, Eq, Generic)

instance ToJSON ListFilesResp where
  toJSON ListFilesResp{..} = object
    [ "workspace_id" .= lfrWorkspaceId
    , "files" .= lfrFiles
    ]
instance FromJSON ListFilesResp
instance ToSchema ListFilesResp


-- ════════════════════════════════════════════════════════════════════════════════
-- AST API Types
-- ════════════════════════════════════════════════════════════════════════════════

data AstSearchReq = AstSearchReq
  { asrWorkspaceId :: !Text
  , asrPattern     :: !Text
  , asrLanguage    :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON AstSearchReq where
  toJSON AstSearchReq{..} = object
    [ "workspace_id" .= asrWorkspaceId
    , "pattern" .= asrPattern
    , "language" .= asrLanguage
    ]
instance FromJSON AstSearchReq where
  parseJSON = withObject "AstSearchReq" $ \v -> AstSearchReq
    <$> v .: "workspace_id"
    <*> v .: "pattern"
    <*> v .:? "language"
instance ToSchema AstSearchReq

data AstReplaceReq = AstReplaceReq
  { arrWorkspaceId  :: !Text
  , arrPattern      :: !Text
  , arrReplacement  :: !Text
  , arrLanguage     :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON AstReplaceReq where
  toJSON AstReplaceReq{..} = object
    [ "workspace_id" .= arrWorkspaceId
    , "pattern" .= arrPattern
    , "replacement" .= arrReplacement
    , "language" .= arrLanguage
    ]
instance FromJSON AstReplaceReq where
  parseJSON = withObject "AstReplaceReq" $ \v -> AstReplaceReq
    <$> v .: "workspace_id"
    <*> v .: "pattern"
    <*> v .: "replacement"
    <*> v .:? "language"
instance ToSchema AstReplaceReq

data AstMatchResp = AstMatchResp
  { amrFile        :: !Text
  , amrLine        :: !Int
  , amrColumn      :: !Int
  , amrText        :: !Text
  , amrReplacement :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON AstMatchResp where
  toJSON AstMatchResp{..} = object
    [ "file" .= amrFile
    , "line" .= amrLine
    , "column" .= amrColumn
    , "text" .= amrText
    , "replacement" .= amrReplacement
    ]
instance FromJSON AstMatchResp
instance ToSchema AstMatchResp

data AstSearchResp = AstSearchResp
  { asrMatches :: ![AstMatchResp]
  , asrCount   :: !Int
  } deriving (Show, Eq, Generic)

instance ToJSON AstSearchResp where
  toJSON AstSearchResp{..} = object ["matches" .= asrMatches, "count" .= asrCount]
instance FromJSON AstSearchResp
instance ToSchema AstSearchResp

data AstReplaceResp = AstReplaceResp
  { arrMatches       :: ![AstMatchResp]
  , arrFilesModified :: !Int
  } deriving (Show, Eq, Generic)

instance ToJSON AstReplaceResp where
  toJSON AstReplaceResp{..} = object
    [ "matches" .= arrMatches
    , "files_modified" .= arrFilesModified
    ]
instance FromJSON AstReplaceResp
instance ToSchema AstReplaceResp


-- ════════════════════════════════════════════════════════════════════════════════
-- Compile API Types
-- ════════════════════════════════════════════════════════════════════════════════

data CompileReq = CompileReq
  { crWorkspaceId :: !Text
  , crPath        :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON CompileReq where
  toJSON CompileReq{..} = object
    [ "workspace_id" .= crWorkspaceId
    , "path" .= crPath
    ]
instance FromJSON CompileReq where
  parseJSON = withObject "CompileReq" $ \v -> CompileReq
    <$> v .: "workspace_id"
    <*> v .: "path"
instance ToSchema CompileReq

data CompileResp = CompileResp
  { cpSuccess   :: !Bool
  , cpLanguage  :: !Text
  , cpStdout    :: !Text
  , cpStderr    :: !Text
  , cpExitCode  :: !Int
  } deriving (Show, Eq, Generic)

instance ToJSON CompileResp where
  toJSON CompileResp{..} = object
    [ "success" .= cpSuccess
    , "language" .= cpLanguage
    , "stdout" .= cpStdout
    , "stderr" .= cpStderr
    , "exit_code" .= cpExitCode
    ]
instance FromJSON CompileResp
instance ToSchema CompileResp


-- ════════════════════════════════════════════════════════════════════════════════
-- Identity API Types
-- ════════════════════════════════════════════════════════════════════════════════

data IdentityResp = IdentityResp
  { irFingerprint :: !Text
  , irDid         :: !Text
  , irPublicKey   :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON IdentityResp where
  toJSON IdentityResp{..} = object
    [ "fingerprint" .= irFingerprint
    , "did" .= irDid
    , "public_key" .= irPublicKey
    ]
instance FromJSON IdentityResp
instance ToSchema IdentityResp


-- ════════════════════════════════════════════════════════════════════════════════
-- Attestation API Types
-- ════════════════════════════════════════════════════════════════════════════════

data CoeffectsReq = CoeffectsReq
  { crqFilesystem :: !(Maybe Text)
  , crqNetwork    :: !(Maybe Text)
  , crqGpu        :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON CoeffectsReq where
  toJSON CoeffectsReq{..} = object
    [ "filesystem" .= crqFilesystem
    , "network" .= crqNetwork
    , "gpu" .= crqGpu
    ]
instance FromJSON CoeffectsReq where
  parseJSON = withObject "CoeffectsReq" $ \v -> CoeffectsReq
    <$> v .:? "filesystem"
    <*> v .:? "network"
    <*> v .:? "gpu"
instance ToSchema CoeffectsReq

data CreateAttestationReq = CreateAttestationReq
  { carAttestType :: !Text
  , carContext    :: !Text
  , carThought    :: !(Maybe Text)
  , carAction     :: !(Maybe Text)
  , carCoeffects  :: !(Maybe CoeffectsReq)
  } deriving (Show, Eq, Generic)

instance ToJSON CreateAttestationReq where
  toJSON CreateAttestationReq{..} = object
    [ "attest_type" .= carAttestType
    , "context" .= carContext
    , "thought" .= carThought
    , "action" .= carAction
    , "coeffects" .= carCoeffects
    ]
instance FromJSON CreateAttestationReq where
  parseJSON = withObject "CreateAttestationReq" $ \v -> CreateAttestationReq
    <$> v .: "attest_type"
    <*> v .: "context"
    <*> v .:? "thought"
    <*> v .:? "action"
    <*> v .:? "coeffects"
instance ToSchema CreateAttestationReq

data SignatureResp = SignatureResp
  { sigStatus :: !Text
  , sigSigner :: !(Maybe Text)
  , sigReason :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON SignatureResp where
  toJSON SignatureResp{..} = object
    [ "status" .= sigStatus
    , "signer" .= sigSigner
    , "reason" .= sigReason
    ]
instance FromJSON SignatureResp
instance ToSchema SignatureResp

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
  toJSON AttestationResp{..} = object
    [ "commit" .= atrCommit
    , "timestamp" .= atrTimestamp
    , "attest_type" .= atrAttestType
    , "context" .= atrContext
    , "thought" .= atrThought
    , "action" .= atrAction
    , "coeffects" .= atrCoeffects
    , "signature" .= atrSignature
    ]
instance FromJSON AttestationResp
instance ToSchema AttestationResp

data AttestationLogResp = AttestationLogResp
  { alrAttestations :: ![AttestationResp]
  , alrCount        :: !Int
  } deriving (Show, Eq, Generic)

instance ToJSON AttestationLogResp where
  toJSON AttestationLogResp{..} = object
    [ "attestations" .= alrAttestations
    , "count" .= alrCount
    ]
instance FromJSON AttestationLogResp
instance ToSchema AttestationLogResp


-- ════════════════════════════════════════════════════════════════════════════════
-- Search/Tools API Types
-- ════════════════════════════════════════════════════════════════════════════════

data SearchResult = SearchResult
  { srtTitle   :: !Text
  , srtUrl     :: !Text
  , srtContent :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON SearchResult where
  toJSON SearchResult{..} = object
    [ "title" .= srtTitle
    , "url" .= srtUrl
    , "content" .= srtContent
    ]
instance FromJSON SearchResult
instance ToSchema SearchResult

data SearchResp = SearchResp
  { srchStatus  :: !Text
  , srchQuery   :: !Text
  , srchResults :: ![SearchResult]
  , srchError   :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON SearchResp where
  toJSON SearchResp{..} = object
    [ "status" .= srchStatus
    , "query" .= srchQuery
    , "results" .= srchResults
    , "error" .= srchError
    ]
instance FromJSON SearchResp
instance ToSchema SearchResp

data ReadUrlResp = ReadUrlResp
  { rurStatus  :: !Text
  , rurUrl     :: !Text
  , rurTitle   :: !(Maybe Text)
  , rurContent :: !Text
  , rurError   :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON ReadUrlResp where
  toJSON ReadUrlResp{..} = object
    [ "status" .= rurStatus
    , "url" .= rurUrl
    , "title" .= rurTitle
    , "content" .= rurContent
    , "error" .= rurError
    ]
instance FromJSON ReadUrlResp
instance ToSchema ReadUrlResp


-- ════════════════════════════════════════════════════════════════════════════════
-- CAS (Content-Addressable Storage) API Types
-- ════════════════════════════════════════════════════════════════════════════════

-- | Digest for content-addressed blobs
data DigestResp = DigestResp
  { drHash :: !Text  -- ^ SHA256 hex (64 chars)
  , drSize :: !Int   -- ^ Size in bytes
  } deriving (Show, Eq, Generic)

instance ToJSON DigestResp where
  toJSON DigestResp{..} = object
    [ "hash" .= drHash
    , "size" .= drSize
    ]
instance FromJSON DigestResp
instance ToSchema DigestResp

-- | Request to upload a blob
data PutBlobReq = PutBlobReq
  { pbrContent :: !Text  -- ^ Base64-encoded content
  } deriving (Show, Eq, Generic)

instance ToJSON PutBlobReq where
  toJSON PutBlobReq{..} = object ["content" .= pbrContent]
instance FromJSON PutBlobReq where
  parseJSON = withObject "PutBlobReq" $ \v ->
    PutBlobReq <$> v .: "content"
instance ToSchema PutBlobReq

-- | Response from putting a blob
data PutBlobResp = PutBlobResp
  { pbrDigest :: !DigestResp
  , pbrStatus :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON PutBlobResp where
  toJSON PutBlobResp{..} = object
    [ "digest" .= pbrDigest
    , "status" .= pbrStatus
    ]
instance FromJSON PutBlobResp
instance ToSchema PutBlobResp

-- | Response from getting a blob
data GetBlobResp = GetBlobResp
  { gbrContent :: !(Maybe Text)  -- ^ Base64-encoded content (Nothing if not found)
  , gbrDigest  :: !DigestResp
  , gbrStatus  :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON GetBlobResp where
  toJSON GetBlobResp{..} = object
    [ "content" .= gbrContent
    , "digest" .= gbrDigest
    , "status" .= gbrStatus
    ]
instance FromJSON GetBlobResp
instance ToSchema GetBlobResp

-- | CAS store info
data CASInfoResp = CASInfoResp
  { cirDataDir    :: !Text
  , cirBlobCount  :: !Int
  , cirNativelink :: !(Maybe Text)  -- ^ NativeLink endpoint if configured
  } deriving (Show, Eq, Generic)

instance ToJSON CASInfoResp where
  toJSON CASInfoResp{..} = object
    [ "data_dir" .= cirDataDir
    , "blob_count" .= cirBlobCount
    , "nativelink" .= cirNativelink
    ]
instance FromJSON CASInfoResp
instance ToSchema CASInfoResp
