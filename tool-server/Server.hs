{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- | Tool Server - Servant-based API server
--
-- Thin handlers over reusable libraries (CodeSandbox, Attestation).
-- Auto-generates OpenAPI3 spec at /openapi.json
--
-- @since 0.1.0

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.OpenApi (OpenApi)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp
import Servant
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout, stderr, hPutStrLn)
import Text.Read (readMaybe)

import API (ToolServerAPIFull, toolServerAPIFull, openApiSpec, HealthResp(..), CoeffectsManifest(..))
import qualified Box
import API.Types
import qualified CodeSandbox as CS
import qualified Attestation as AT
import qualified CAS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as TE


-- ════════════════════════════════════════════════════════════════════════════════
-- Server State
-- ════════════════════════════════════════════════════════════════════════════════

data ServerState = ServerState
  { ssSandbox        :: !CS.SandboxState
  , ssIdentity       :: !(Maybe AT.Identity)
  , ssAttestationDir :: !(Maybe FilePath)
  , ssCASStore       :: !CAS.CASStore
  , ssManager        :: !Manager
  , ssSearxngUrl     :: !(Maybe Text)
  , ssJinaApiKey     :: !(Maybe Text)
  }

initServerState :: IO ServerState
initServerState = do
  sandbox <- CS.newSandboxState
  identityDir <- lookupEnv "IDENTITY_DIR"
  attestDir <- lookupEnv "ATTESTATION_DIR"
  identity <- either (const Nothing) Just <$> AT.loadIdentity identityDir
  -- CAS store - defaults to /var/lib/trtllm/cas
  casDir <- fromMaybe "/var/lib/trtllm/cas" <$> lookupEnv "CAS_DATA_DIR"
  casNativelink <- lookupEnv "CAS_NATIVELINK_ENDPOINT"
  let casConfig = case casNativelink of
        Nothing -> CAS.defaultConfig casDir
        Just ep -> CAS.nativelinkConfig casDir ep
  casStore <- CAS.newCASStore casConfig
  manager <- newManager tlsManagerSettings
  searxng <- fmap T.pack <$> lookupEnv "SEARXNG_URL"
  jina <- fmap T.pack <$> lookupEnv "JINA_API_KEY"
  pure $ ServerState sandbox identity attestDir casStore manager searxng jina


-- ════════════════════════════════════════════════════════════════════════════════
-- Handlers
-- ════════════════════════════════════════════════════════════════════════════════

-- Health handlers
healthHandler :: Handler HealthResp
healthHandler = pure $ HealthResp
  { hrStatus = "ok"
  , hrVersion = "0.1.0"
  , hrComponents = ["code-sandbox", "attestation", "tools"]
  }

openApiHandler :: Handler OpenApi
openApiHandler = pure openApiSpec

-- Code sandbox handlers
createWorkspaceHandler :: ServerState -> CreateWorkspaceReq -> Handler WorkspaceResp
createWorkspaceHandler state CreateWorkspaceReq{..} = do
  let mWsId = CS.WorkspaceId <$> cwrWorkspaceId
  ws <- liftIO $ CS.createWorkspace (ssSandbox state) mWsId
  pure $ WorkspaceResp (CS.unWorkspaceId $ CS.wsId ws) (T.pack $ CS.wsPath ws)

getWorkspaceHandler :: ServerState -> Text -> Handler WorkspaceResp
getWorkspaceHandler state wsId = do
  mWs <- liftIO $ CS.getWorkspace (ssSandbox state) (CS.WorkspaceId wsId)
  case mWs of
    Nothing -> throwError err404 { errBody = "Workspace not found" }
    Just ws -> pure $ WorkspaceResp (CS.unWorkspaceId $ CS.wsId ws) (T.pack $ CS.wsPath ws)

listWorkspacesHandler :: ServerState -> Handler ListWorkspacesResp
listWorkspacesHandler state = do
  wss <- liftIO $ CS.listWorkspaces (ssSandbox state)
  let resp = [WorkspaceResp (CS.unWorkspaceId $ CS.wsId ws) (T.pack $ CS.wsPath ws) | ws <- wss]
  pure $ ListWorkspacesResp resp

writeFileHandler :: ServerState -> WriteFileReq -> Handler SuccessResp
writeFileHandler state WriteFileReq{..} = do
  mWs <- liftIO $ CS.getWorkspace (ssSandbox state) (CS.WorkspaceId wfrWorkspaceId)
  case mWs of
    Nothing -> throwError err404 { errBody = "Workspace not found" }
    Just ws -> do
      result <- liftIO $ CS.writeFile ws (T.unpack wfrPath) wfrContent
      case result of
        Left _err -> throwError err500 { errBody = "Write failed" }
        Right () -> pure $ SuccessResp "ok"

readFileHandler :: ServerState -> ReadFileReq -> Handler FileContentResp
readFileHandler state ReadFileReq{..} = do
  mWs <- liftIO $ CS.getWorkspace (ssSandbox state) (CS.WorkspaceId rfrWorkspaceId)
  case mWs of
    Nothing -> throwError err404 { errBody = "Workspace not found" }
    Just ws -> do
      result <- liftIO $ CS.readFile ws (T.unpack rfrPath)
      case result of
        Left _err -> throwError err404 { errBody = "File not found" }
        Right content -> pure $ FileContentResp rfrPath content

listFilesHandler :: ServerState -> Text -> Handler ListFilesResp
listFilesHandler state wsId = do
  mWs <- liftIO $ CS.getWorkspace (ssSandbox state) (CS.WorkspaceId wsId)
  case mWs of
    Nothing -> throwError err404 { errBody = "Workspace not found" }
    Just ws -> do
      files <- liftIO $ CS.listFiles ws
      pure $ ListFilesResp wsId (map T.pack files)

astSearchHandler :: ServerState -> AstSearchReq -> Handler AstSearchResp
astSearchHandler state AstSearchReq{..} = do
  mWs <- liftIO $ CS.getWorkspace (ssSandbox state) (CS.WorkspaceId asrWorkspaceId)
  case mWs of
    Nothing -> throwError err404 { errBody = "Workspace not found" }
    Just ws -> do
      let mLang = asrLanguage >>= parseLang
      result <- liftIO $ CS.astSearch ws asrPattern mLang
      case result of
        Left _err -> throwError err500 { errBody = "AST search failed" }
        Right matches -> pure $ AstSearchResp 
          (map convertMatch matches) 
          (length matches)

astReplaceHandler :: ServerState -> AstReplaceReq -> Handler AstReplaceResp
astReplaceHandler state AstReplaceReq{..} = do
  mWs <- liftIO $ CS.getWorkspace (ssSandbox state) (CS.WorkspaceId arrWorkspaceId)
  case mWs of
    Nothing -> throwError err404 { errBody = "Workspace not found" }
    Just ws -> do
      let mLang = arrLanguage >>= parseLang
      result <- liftIO $ CS.astReplace ws arrPattern arrReplacement mLang
      case result of
        Left _err -> throwError err500 { errBody = "AST replace failed" }
        Right (matches, count) -> pure $ AstReplaceResp 
          (map convertMatch matches) 
          count

compileHandler :: ServerState -> CompileReq -> Handler CompileResp
compileHandler state CompileReq{..} = do
  mWs <- liftIO $ CS.getWorkspace (ssSandbox state) (CS.WorkspaceId crWorkspaceId)
  case mWs of
    Nothing -> throwError err404 { errBody = "Workspace not found" }
    Just ws -> do
      result <- liftIO $ CS.compile ws (T.unpack crPath)
      case result of
        Left _err -> throwError err500 { errBody = "Compile failed" }
        Right cr -> pure $ CompileResp
          (CS.crSuccess cr)
          (langToText $ CS.crLanguage cr)
          (CS.crStdout cr)
          (CS.crStderr cr)
          (CS.crExitCode cr)

-- Identity handlers
identityInfoHandler :: ServerState -> Handler IdentityResp
identityInfoHandler state = case ssIdentity state of
  Nothing -> throwError err500 { errBody = "Identity not configured" }
  Just AT.Identity{..} -> pure $ IdentityResp idFingerprint idDID idPublicKey

-- Attestation handlers
createAttestationHandler :: ServerState -> CreateAttestationReq -> Handler AttestationResp
createAttestationHandler state CreateAttestationReq{..} = case ssIdentity state of
  Nothing -> throwError err500 { errBody = "Identity not configured" }
  Just identity -> do
    let attType = parseAttestType carAttestType
        coeffs = maybe emptyCoeffs toCoeffs carCoeffects
    result <- liftIO $ AT.createAttestation 
      identity Nothing attType carContext carThought carAction coeffs
    case result of
      Left _err -> throwError err500 { errBody = "Attestation failed" }
      Right att -> pure $ convertAttestation att

attestationLogHandler :: ServerState -> Maybe Int -> Handler AttestationLogResp
attestationLogHandler state mLimit = do
  let limit = fromMaybe 20 mLimit
  result <- liftIO $ AT.getAttestationLog (ssAttestationDir state) limit
  case result of
    Left err -> do
      throwError err500 { errBody = "Failed to read attestation log" }
    Right atts -> pure $ AttestationLogResp (map convertAttestation atts) (length atts)

verifyAttestationHandler :: ServerState -> Text -> Handler SignatureResp
verifyAttestationHandler state commit = do
  result <- liftIO $ AT.verifyAttestation (ssAttestationDir state) commit
  case result of
    Left _err -> throwError err500 { errBody = "Verification failed" }
    Right status -> pure $ convertSigStatus status

-- Tools handlers
searchHandler :: ServerState -> Maybe Text -> Maybe Int -> Handler SearchResp
searchHandler state mQuery _mNum = case (ssSearxngUrl state, mQuery) of
  (Nothing, _) -> pure $ SearchResp "error" "" [] (Just "SEARXNG_URL not configured")
  (_, Nothing) -> pure $ SearchResp "error" "" [] (Just "query parameter required")
  (Just _baseUrl, Just query) -> do
    -- TODO: Implement SearXNG search
    pure $ SearchResp "ok" query [] Nothing

readUrlHandler :: ServerState -> Maybe Text -> Handler ReadUrlResp
readUrlHandler _state mUrl = case mUrl of
  Nothing -> pure $ ReadUrlResp "error" "" Nothing "" (Just "url parameter required")
  Just url -> do
    -- TODO: Implement Jina reader
    pure $ ReadUrlResp "ok" url Nothing "" Nothing

-- Coeffects manifest
coeffectsManifestHandler :: Handler CoeffectsManifest
coeffectsManifestHandler = pure $ CoeffectsManifest
  (KM.fromList
    [ ("code/compile", object ["filesystem" .= ("readWrite" :: Text)])
    , ("code/ast_search", object ["filesystem" .= ("read" :: Text)])
    , ("code/ast_replace", object ["filesystem" .= ("readWrite" :: Text)])
    , ("attest", object ["filesystem" .= ("readWrite" :: Text), "crypto" .= ("sign" :: Text)])
    , ("tools/search", object ["network" .= ("read" :: Text)])
    , ("tools/read_url", object ["network" .= ("read" :: Text)])
    ])
  "Coeffect semiring: (R, join, 0, meet, 1)"


-- ════════════════════════════════════════════════════════════════════════════════
-- Helpers
-- ════════════════════════════════════════════════════════════════════════════════

parseLang :: Text -> Maybe CS.Language
parseLang t = case T.toLower t of
  "rust"       -> Just CS.Rust
  "haskell"    -> Just CS.Haskell
  "lean"       -> Just CS.Lean
  "dhall"      -> Just CS.Dhall
  "purescript" -> Just CS.PureScript
  "purs"       -> Just CS.PureScript
  _            -> Nothing

langToText :: CS.Language -> Text
langToText = \case
  CS.Rust       -> "rust"
  CS.Haskell    -> "haskell"
  CS.Lean       -> "lean"
  CS.Dhall      -> "dhall"
  CS.PureScript -> "purescript"

convertMatch :: CS.AstMatch -> AstMatchResp
convertMatch CS.AstMatch{..} = AstMatchResp amFile amLine amColumn amText amReplacement

parseAttestType :: Text -> AT.AttestationType
parseAttestType t = case T.toLower t of
  "task"      -> AT.AttestTask
  "reasoning" -> AT.AttestReasoning
  "decision"  -> AT.AttestDecision
  "coeffect"  -> AT.AttestCoeffect
  custom      -> AT.AttestCustom custom

emptyCoeffs :: AT.Coeffects
emptyCoeffs = AT.Coeffects Nothing Nothing Nothing []

toCoeffs :: CoeffectsReq -> AT.Coeffects
toCoeffs cr = AT.Coeffects (crqFilesystem cr) (crqNetwork cr) (crqGpu cr) []

convertAttestation :: AT.Attestation -> AttestationResp
convertAttestation AT.Attestation{..} = AttestationResp
  atCommitHash
  (T.pack $ show atTimestamp)  -- TODO: proper ISO8601
  (attTypeToText atType)
  atContext
  atThought
  atAction
  (coeffsToResp atCoeffects)
  (convertSigStatus atSignature)

attTypeToText :: AT.AttestationType -> Text
attTypeToText = \case
  AT.AttestTask       -> "task"
  AT.AttestReasoning  -> "reasoning"
  AT.AttestDecision   -> "decision"
  AT.AttestCoeffect   -> "coeffect"
  AT.AttestCustom t   -> t

coeffsToResp :: AT.Coeffects -> CoeffectsReq
coeffsToResp c = CoeffectsReq (AT.coFilesystem c) (AT.coNetwork c) (AT.coGpu c)

convertSigStatus :: AT.SignatureStatus -> SignatureResp
convertSigStatus = \case
  AT.SigGood fp -> SignatureResp "good" (Just fp) Nothing
  AT.SigBad r   -> SignatureResp "bad" Nothing (Just r)
  AT.SigUnknown -> SignatureResp "unknown" Nothing Nothing
  AT.SigNone    -> SignatureResp "none" Nothing Nothing


-- ════════════════════════════════════════════════════════════════════════════════
-- CAS Handlers
-- ════════════════════════════════════════════════════════════════════════════════

casInfoHandler :: ServerState -> Handler CASInfoResp
casInfoHandler state = do
  let config = CAS.storeConfig (ssCASStore state)
  blobs <- liftIO $ CAS.listBlobs (ssCASStore state)
  pure $ CASInfoResp
    { cirDataDir = T.pack (CAS.casDataDir config)
    , cirBlobCount = length blobs
    , cirNativelink = T.pack <$> CAS.casNativelinkEndpoint config
    }

casPutBlobHandler :: ServerState -> PutBlobReq -> Handler PutBlobResp
casPutBlobHandler state PutBlobReq{..} = do
  -- Decode base64 content
  case B64.decode (TE.encodeUtf8 pbrContent) of
    Left err -> throwError $ err400 { errBody = "Invalid base64: " <> BS.fromStrict (TE.encodeUtf8 $ T.pack err) }
    Right content -> do
      digest <- liftIO $ CAS.putBlob (ssCASStore state) content
      pure $ PutBlobResp
        { pbrDigest = digestToResp digest
        , pbrStatus = "stored"
        }

casGetBlobHandler :: ServerState -> Text -> Handler GetBlobResp
casGetBlobHandler state hashText = do
  -- Parse digest from hash (we don't have size, so we need to look it up)
  mContent <- liftIO $ do
    blobs <- CAS.listBlobs (ssCASStore state)
    case filter (\d -> CAS.digestHash d == hashText) blobs of
      []    -> pure Nothing
      (d:_) -> CAS.getBlob (ssCASStore state) d
  case mContent of
    Nothing -> pure $ GetBlobResp
      { gbrContent = Nothing
      , gbrDigest = DigestResp hashText 0
      , gbrStatus = "not_found"
      }
    Just content -> do
      let digest = CAS.digestFromBytes content
      pure $ GetBlobResp
        { gbrContent = Just $ TE.decodeUtf8 $ B64.encode content
        , gbrDigest = digestToResp digest
        , gbrStatus = "found"
        }

casExistsHandler :: ServerState -> Text -> Handler SuccessResp
casExistsHandler state hashText = do
  blobs <- liftIO $ CAS.listBlobs (ssCASStore state)
  let exists = any (\d -> CAS.digestHash d == hashText) blobs
  if exists
    then pure $ SuccessResp "exists"
    else pure $ SuccessResp "not_found"

digestToResp :: CAS.Digest -> DigestResp
digestToResp d = DigestResp (CAS.digestHash d) (CAS.digestSize d)


-- ════════════════════════════════════════════════════════════════════════════════
-- Box Handlers - Pure, No IO, The Result Is Saved
-- ════════════════════════════════════════════════════════════════════════════════

boxTableHandler :: BoxTableReq -> Handler BoxResp
boxTableHandler BoxTableReq{..} = do
  let style = parseBoxStyle btrStyle
      tableData = Box.TableData btrHeaders btrRows style
  pure $ BoxResp (Box.renderTable tableData)

boxFrameHandler :: BoxFrameReq -> Handler BoxResp
boxFrameHandler BoxFrameReq{..} = do
  let style = parseBoxStyle bfrStyle
      frameData = Box.FrameData bfrTitle bfrContent bfrWidth style
  pure $ BoxResp (Box.renderFrame frameData)

boxTreeHandler :: BoxTreeReq -> Handler BoxResp
boxTreeHandler BoxTreeReq{..} = do
  let tree = Box.TreeNode btqRoot (map convertTreeNode btqChildren)
  pure $ BoxResp (Box.renderTree tree)

boxDiagramHandler :: BoxDiagramReq -> Handler BoxResp
boxDiagramHandler BoxDiagramReq{..} = do
  let style = parseBoxStyle bdrStyle
      layout = parseLayout bdrLayout
      nodes = map convertDiagramNode bdrNodes
      edges = map convertDiagramEdge bdrEdges
      diagramData = Box.DiagramData nodes edges layout style
  pure $ BoxResp (Box.renderDiagram diagramData)

parseBoxStyle :: Maybe Text -> Box.BoxStyle
parseBoxStyle Nothing = Box.Single
parseBoxStyle (Just t) = case T.toLower t of
  "double"  -> Box.Double
  "rounded" -> Box.Rounded
  _         -> Box.Single

parseLayout :: Maybe Text -> Box.DiagramLayout
parseLayout Nothing = Box.Horizontal
parseLayout (Just t) = case T.toLower t of
  "vertical" -> Box.Vertical
  "flow"     -> Box.Flow
  _          -> Box.Horizontal

convertTreeNode :: BoxTreeNodeReq -> Box.TreeNode
convertTreeNode BoxTreeNodeReq{..} = 
  Box.TreeNode btnLabel (map convertTreeNode btnChildren)

convertDiagramNode :: BoxDiagramNodeReq -> Box.DiagramNode
convertDiagramNode BoxDiagramNodeReq{..} =
  Box.DiagramNode bdnId bdnLabel

convertDiagramEdge :: BoxDiagramEdgeReq -> Box.DiagramEdge
convertDiagramEdge BoxDiagramEdgeReq{..} =
  Box.DiagramEdge bdeFrom bdeTo bdeLabel


-- ════════════════════════════════════════════════════════════════════════════════
-- Server
-- ════════════════════════════════════════════════════════════════════════════════

server :: ServerState -> Server ToolServerAPIFull
server state = 
       -- Health
       (healthHandler :<|> openApiHandler)
       -- Code
  :<|> (createWorkspaceHandler state
   :<|> getWorkspaceHandler state
   :<|> listWorkspacesHandler state
   :<|> writeFileHandler state
   :<|> readFileHandler state
   :<|> listFilesHandler state
   :<|> astSearchHandler state
   :<|> astReplaceHandler state
   :<|> compileHandler state)
       -- Identity
  :<|> (identityInfoHandler state
   :<|> createAttestationHandler state
   :<|> attestationLogHandler state
   :<|> verifyAttestationHandler state)
       -- Tools
  :<|> (searchHandler state
   :<|> searchHandler state  -- code_search uses same handler
   :<|> readUrlHandler state)
       -- CAS
  :<|> (casInfoHandler state
   :<|> casPutBlobHandler state
   :<|> casGetBlobHandler state
   :<|> casExistsHandler state)
       -- Box (pure, no state needed)
  :<|> (boxTableHandler
   :<|> boxFrameHandler
   :<|> boxTreeHandler
   :<|> boxDiagramHandler)
       -- Coeffects
  :<|> coeffectsManifestHandler

app :: ServerState -> Application
app state = serve toolServerAPIFull (server state)


-- ════════════════════════════════════════════════════════════════════════════════
-- Main
-- ════════════════════════════════════════════════════════════════════════════════

main :: IO ()
main = do
  port <- maybe 9001 id . (>>= readMaybe) <$> lookupEnv "TOOL_SERVER_PORT"
  state <- initServerState
  
  putStrLn $ replicate 72 '='
  putStrLn "  Tool Server (Servant + OpenAPI3)"
  putStrLn $ replicate 72 '='
  putStrLn ""
  putStrLn $ "  Port: " ++ show port
  putStrLn ""
  putStrLn "  Endpoints:"
  putStrLn $ "    GET  /health           - Health check"
  putStrLn $ "    GET  /openapi.json     - OpenAPI 3.0 spec"
  putStrLn ""
  putStrLn "  Code Sandbox:"
  putStrLn $ "    POST /code/workspace   - Create workspace"
  putStrLn $ "    POST /code/write       - Write file"
  putStrLn $ "    POST /code/read        - Read file"
  putStrLn $ "    POST /code/ast_search  - AST pattern search"
  putStrLn $ "    POST /code/ast_replace - AST pattern replace"
  putStrLn $ "    POST /code/compile     - Compile file"
  putStrLn ""
  putStrLn "  Identity & Attestation:"
  putStrLn $ "    GET  /identity/info    - Get agent identity"
  putStrLn $ "    POST /attest           - Create attestation"
  putStrLn $ "    GET  /attest/log       - Get attestation log"
  putStrLn ""
  putStrLn "  Tools:"
  putStrLn $ "    GET  /tools/search     - Web search"
  putStrLn $ "    GET  /tools/read_url   - Read URL content"
  putStrLn ""
  case ssIdentity state of
    Nothing -> putStrLn "  Identity: NOT CONFIGURED"
    Just ident -> do
      putStrLn $ "  Identity: " ++ T.unpack (AT.idFingerprint ident)
      putStrLn $ "  DID: " ++ T.unpack (AT.idDID ident)
  putStrLn ""
  putStrLn $ replicate 72 '='
  hFlush stdout
  
  run port (app state)
