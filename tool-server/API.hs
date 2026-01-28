{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Servant API Definition for Tool Server
--
-- Type-level API definition with automatic OpenAPI3 spec generation.
--
-- @since 0.1.0

module API
  ( -- * Combined API
    ToolServerAPI
  , toolServerAPI
    -- * Sub-APIs
  , CodeAPI
  , IdentityAPI
  , ToolsAPI
  , HealthAPI
    -- * Response types defined here
  , HealthResp(..)
  , CoeffectsManifest(..)
    -- * OpenAPI
  , openApiSpec
  ) where

import Control.Lens ((&), (.~), (?~))
import Data.Aeson (ToJSON, FromJSON, Object)
import Data.OpenApi (OpenApi, ToSchema, info, title, version, description)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
import Servant.OpenApi (toOpenApi)

import API.Types


-- ════════════════════════════════════════════════════════════════════════════════
-- Health API
-- ════════════════════════════════════════════════════════════════════════════════

type HealthAPI =
       "health" :> Get '[JSON] HealthResp
  :<|> "openapi.json" :> Get '[JSON] OpenApi

data HealthResp = HealthResp
  { hrStatus     :: !Text
  , hrVersion    :: !Text
  , hrComponents :: ![Text]
  } deriving (Show, Eq, Generic)

instance ToJSON HealthResp
instance FromJSON HealthResp
instance ToSchema HealthResp


-- ════════════════════════════════════════════════════════════════════════════════
-- Code Sandbox API
-- ════════════════════════════════════════════════════════════════════════════════

type CodeAPI = "code" :>
  (    -- Workspace management
       "workspace" :> ReqBody '[JSON] CreateWorkspaceReq :> Post '[JSON] WorkspaceResp
  :<|> "workspace" :> Capture "id" Text :> Get '[JSON] WorkspaceResp
  :<|> "workspaces" :> Get '[JSON] ListWorkspacesResp
  
       -- File operations
  :<|> "write" :> ReqBody '[JSON] WriteFileReq :> Post '[JSON] SuccessResp
  :<|> "read" :> ReqBody '[JSON] ReadFileReq :> Post '[JSON] FileContentResp
  :<|> "files" :> Capture "workspace_id" Text :> Get '[JSON] ListFilesResp
  
       -- AST operations
  :<|> "ast_search" :> ReqBody '[JSON] AstSearchReq :> Post '[JSON] AstSearchResp
  :<|> "ast_replace" :> ReqBody '[JSON] AstReplaceReq :> Post '[JSON] AstReplaceResp
  
       -- Compilation
  :<|> "compile" :> ReqBody '[JSON] CompileReq :> Post '[JSON] CompileResp
  )


-- ════════════════════════════════════════════════════════════════════════════════
-- Identity & Attestation API
-- ════════════════════════════════════════════════════════════════════════════════

type IdentityAPI = 
  (    -- Identity info
       "identity" :> "info" :> Get '[JSON] IdentityResp
  
       -- Attestation operations
  :<|> "attest" :> ReqBody '[JSON] CreateAttestationReq :> Post '[JSON] AttestationResp
  :<|> "attest" :> "log" :> QueryParam "limit" Int :> Get '[JSON] AttestationLogResp
  :<|> "attest" :> Capture "commit" Text :> "verify" :> Get '[JSON] SignatureResp
  )


-- ════════════════════════════════════════════════════════════════════════════════
-- Tools API (Search, URL Reader)
-- ════════════════════════════════════════════════════════════════════════════════

type ToolsAPI = "tools" :>
  (    "search" :> QueryParam "query" Text :> QueryParam "num_results" Int :> Get '[JSON] SearchResp
  :<|> "code_search" :> QueryParam "query" Text :> QueryParam "num_results" Int :> Get '[JSON] SearchResp
  :<|> "read_url" :> QueryParam "url" Text :> Get '[JSON] ReadUrlResp
  )


-- ════════════════════════════════════════════════════════════════════════════════
-- Coeffects Manifest
-- ════════════════════════════════════════════════════════════════════════════════

type CoeffectsAPI = "coeffects" :>
  (    "manifest" :> Get '[JSON] CoeffectsManifest
  )

data CoeffectsManifest = CoeffectsManifest
  { cmTools   :: !Object
  , cmAlgebra :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON CoeffectsManifest
instance FromJSON CoeffectsManifest
instance ToSchema CoeffectsManifest


-- ════════════════════════════════════════════════════════════════════════════════
-- Combined API
-- ════════════════════════════════════════════════════════════════════════════════

type ToolServerAPI = 
       HealthAPI 
  :<|> CodeAPI 
  :<|> IdentityAPI 
  :<|> ToolsAPI
  :<|> CoeffectsAPI

toolServerAPI :: Proxy ToolServerAPI
toolServerAPI = Proxy


-- ════════════════════════════════════════════════════════════════════════════════
-- OpenAPI Spec
-- ════════════════════════════════════════════════════════════════════════════════

openApiSpec :: OpenApi
openApiSpec = toOpenApi toolServerAPI
  & info . title .~ "Tool Server API"
  & info . version .~ "0.1.0"
  & info . description ?~ 
      "Code sandbox, attestation, and tool endpoints for AI agents.\n\n\
      \**Features:**\n\
      \- Workspace management for isolated code editing\n\
      \- AST-based search/replace via ast-grep\n\
      \- Compilation for Rust, Haskell, Lean, Dhall, PureScript\n\
      \- Ed25519 identity and signed attestations\n\
      \- Web search and URL reading tools"
