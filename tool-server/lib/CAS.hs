{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : CAS
Description : Content-Addressed Storage with NativeLink backend

Local filesystem CAS with optional NativeLink remote backend.
Uses SHA256 digests for content addressing.

This is a simplified implementation that stores blobs locally.
For production, configure a NativeLink endpoint for distributed storage.

The design follows the Remote Execution API:
  https://github.com/bazelbuild/remote-apis
-}
module CAS (
    -- * Configuration
    CASConfig (..),
    defaultConfig,
    nativelinkConfig,

    -- * Store
    CASStore (..),  -- Export record fields for access
    newCASStore,
    withCASStore,

    -- * Digest
    Digest (..),
    digestFromBytes,
    digestFromText,
    digestToPath,

    -- * Operations
    putBlob,
    getBlob,
    hasBlob,
    deleteBlob,
    listBlobs,

    -- * Utilities
    hashBytes,
) where

import Control.Exception (bracket, try, SomeException)
import Control.Monad (unless, when)
import Crypto.Hash (SHA256 (..), hashWith)
import qualified Data.ByteArray.Encoding as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, removeFile)
import System.FilePath ((</>), takeDirectory)
import Data.Aeson (ToJSON, FromJSON)

-- -----------------------------------------------------------------------------
-- Configuration
-- -----------------------------------------------------------------------------

-- | CAS configuration
data CASConfig = CASConfig
    { casDataDir :: FilePath
    -- ^ Local storage directory for blobs
    , casNativelinkEndpoint :: Maybe String
    -- ^ Optional NativeLink gRPC endpoint (e.g., "grpc://cas.straylight.cx:50052")
    , casInstanceName :: Text
    -- ^ Remote execution instance name (usually "main")
    , casMaxLocalBytes :: Integer
    -- ^ Maximum local cache size in bytes (for eviction)
    }
    deriving (Show, Eq, Generic)

instance ToJSON CASConfig
instance FromJSON CASConfig

-- | Default config for local-only storage
defaultConfig :: FilePath -> CASConfig
defaultConfig dataDir = CASConfig
    { casDataDir = dataDir
    , casNativelinkEndpoint = Nothing
    , casInstanceName = "main"
    , casMaxLocalBytes = 10 * 1024 * 1024 * 1024 -- 10GB
    }

-- | Config for NativeLink backend
nativelinkConfig :: FilePath -> String -> CASConfig
nativelinkConfig dataDir endpoint = CASConfig
    { casDataDir = dataDir
    , casNativelinkEndpoint = Just endpoint
    , casInstanceName = "main"
    , casMaxLocalBytes = 10 * 1024 * 1024 * 1024
    }

-- -----------------------------------------------------------------------------
-- Digest
-- -----------------------------------------------------------------------------

{- | Content digest (hash + size)

Matches the Remote Execution API Digest message:
  message Digest {
    string hash = 1;      // SHA256 hex
    int64 size_bytes = 2;
  }
-}
data Digest = Digest
    { digestHash :: Text
    -- ^ SHA256 hash as lowercase hex string (64 chars)
    , digestSize :: Int
    -- ^ Size in bytes
    }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON Digest
instance FromJSON Digest

-- | Compute digest from bytes
digestFromBytes :: ByteString -> Digest
digestFromBytes bs = Digest
    { digestHash = hashBytes bs
    , digestSize = BS.length bs
    }

-- | Parse digest from "hash/size" text format
digestFromText :: Text -> Maybe Digest
digestFromText t = case T.splitOn "/" t of
    [h, s] | T.length h == 64 -> 
        case reads (T.unpack s) of
            [(size, "")] -> Just $ Digest h size
            _ -> Nothing
    _ -> Nothing

-- | Convert digest to storage path (hash-based sharding)
digestToPath :: FilePath -> Digest -> FilePath
digestToPath baseDir Digest{..} =
    let h = T.unpack digestHash
        -- Shard by first 2 chars of hash
        shard = take 2 h
    in baseDir </> "blobs" </> shard </> h

-- | Hash bytes to SHA256 hex string
hashBytes :: ByteString -> Text
hashBytes bs =
    let digest = hashWith SHA256 bs
    in TE.decodeUtf8 $ BA.convertToBase BA.Base16 digest

-- -----------------------------------------------------------------------------
-- Store
-- -----------------------------------------------------------------------------

-- | CAS store handle
data CASStore = CASStore
    { storeConfig :: CASConfig
    , storeIndex :: IORef (Map Digest Integer)  -- digest -> access time (for LRU)
    }

-- | Create a new CAS store
newCASStore :: CASConfig -> IO CASStore
newCASStore config = do
    createDirectoryIfMissing True (casDataDir config </> "blobs")
    indexRef <- newIORef Map.empty
    pure $ CASStore config indexRef

-- | Use a CAS store with cleanup
withCASStore :: CASConfig -> (CASStore -> IO a) -> IO a
withCASStore config = bracket (newCASStore config) (const $ pure ())

-- -----------------------------------------------------------------------------
-- Operations
-- -----------------------------------------------------------------------------

-- | Store a blob, returns its digest
putBlob :: CASStore -> ByteString -> IO Digest
putBlob store content = do
    let digest = digestFromBytes content
        path = digestToPath (casDataDir $ storeConfig store) digest
    
    -- Create parent directory
    createDirectoryIfMissing True (takeDirectory path)
    
    -- Write if not exists (content-addressed = idempotent)
    exists <- doesFileExist path
    unless exists $ BS.writeFile path content
    
    -- Update index
    modifyIORef' (storeIndex store) (Map.insert digest 0)
    
    -- TODO: If NativeLink endpoint configured, upload to remote
    -- case casNativelinkEndpoint (storeConfig store) of
    --     Just endpoint -> uploadToNativelink endpoint digest content
    --     Nothing -> pure ()
    
    pure digest

-- | Get a blob by digest
getBlob :: CASStore -> Digest -> IO (Maybe ByteString)
getBlob store digest = do
    let path = digestToPath (casDataDir $ storeConfig store) digest
    exists <- doesFileExist path
    if exists
        then do
            content <- BS.readFile path
            -- Verify integrity
            let actual = digestFromBytes content
            if actual == digest
                then pure (Just content)
                else do
                    -- Corrupted, remove it
                    removeFile path
                    pure Nothing
        else do
            -- TODO: If NativeLink endpoint configured, fetch from remote
            pure Nothing

-- | Check if a blob exists
hasBlob :: CASStore -> Digest -> IO Bool
hasBlob store digest = do
    let path = digestToPath (casDataDir $ storeConfig store) digest
    doesFileExist path

-- | Delete a blob
deleteBlob :: CASStore -> Digest -> IO Bool
deleteBlob store digest = do
    let path = digestToPath (casDataDir $ storeConfig store) digest
    exists <- doesFileExist path
    when exists $ do
        result <- try @SomeException $ removeFile path
        case result of
            Left _ -> pure ()
            Right _ -> modifyIORef' (storeIndex store) (Map.delete digest)
    pure exists

-- | List all stored digests
listBlobs :: CASStore -> IO [Digest]
listBlobs store = do
    index <- readIORef (storeIndex store)
    pure $ Map.keys index
