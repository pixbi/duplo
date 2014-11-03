{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

module Development.Duplo.ComponentIO
  ( AppInfo(..)
  , name
  , repo
  , version
  , appId
  , parseComponentId
  , readManifest
  , writeManifest
  ) where

import Control.Applicative ((<$>), (<*>))
import Development.Shake
import Data.Aeson ((.:), encode, decode, FromJSON, ToJSON, Object)
import GHC.Generics (Generic)
import Data.Text (breakOn)
import qualified Data.Text as T (unpack, pack)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS (unpack, pack)
import System.FilePath.Posix (splitDirectories)
import Control.Monad.IO.Class (MonadIO)
import Data.HashMap.Strict (HashMap, empty)

-- | Each application must have a `component.json`
manifestPath :: String
manifestPath = "component.json"

data AppInfo = AppInfo { name         :: String
                       , repo         :: String
                       , version      :: String
                       , dependencies :: HashMap String String
                       , images       :: [String]
                       , scripts      :: [String]
                       , styles       :: [String]
                       , templates    :: [String]
                       , fonts        :: [String]
                       } deriving (Show, Generic)

-- | Instances for handling the manifest file
instance FromJSON AppInfo
instance ToJSON AppInfo

-- | Need an IO for Maybe type to lift
instance MonadIO Maybe

readManifest :: IO AppInfo
readManifest = do
    manifest <- readFile manifestPath
    let appInfo = decode (BS.pack manifest) :: Maybe AppInfo

    case appInfo of
      Just info -> return info
      -- TODO: use MaybeT
      Nothing   -> return $ AppInfo { name    = ""
                                    , version = ""
                                    , repo    = "owner/repo"
                                    , dependencies = empty
                                    , images  = []
                                    , scripts = []
                                    , styles  = []
                                    , templates = []
                                    , fonts = []
                                    }

writeManifest :: AppInfo -> IO ()
writeManifest = (writeFile manifestPath) . BS.unpack . encode

-- | Get the app's Component.IO ID
appId :: AppInfo -> String
appId appInfo =
    owner ++ "-" ++ appRepo'
  where
    appRepo           = repo appInfo
    (owner : appRepo' : _) = splitDirectories appRepo

-- | Given a possible component ID, return the user and the repo
-- constituents
parseComponentId :: String -> Maybe (String, String)
parseComponentId cId
  | repoL > 0 = Just ((T.unpack user), (T.unpack repo))
  | otherwise = Nothing
  where
    (user, repo) = breakOn (T.pack "-") (T.pack cId)
    repoL = length $ T.unpack repo
