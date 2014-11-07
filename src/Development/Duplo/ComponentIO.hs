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
import Development.Shake hiding (doesFileExist)
import Data.Aeson ((.:), encode, decode, FromJSON, ToJSON, Object)
import GHC.Generics (Generic)
import Data.Text (breakOn)
import qualified Data.Text as T (unpack, pack)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS (unpack, pack)
import System.FilePath.Posix (splitDirectories)
import Data.HashMap.Strict (HashMap, empty)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Class (lift)
import System.Directory (doesFileExist)

-- | Each application must have a `component.json`
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

readManifest :: MaybeT IO AppInfo
readManifest = do
    exists <- liftIO $ doesFileExist manifestPath

    if   exists
    then readManifest' manifestPath
    else MaybeT $ return Nothing

readManifest' :: FilePath -> MaybeT IO AppInfo
readManifest' path = do
    manifest <- liftIO $ readFile path
    let maybeAppInfo = decode (BS.pack manifest) :: Maybe AppInfo

    case maybeAppInfo of
      Nothing -> MaybeT $ return Nothing
      Just a  -> MaybeT $ return $ Just a

writeManifest :: AppInfo -> IO ()
writeManifest = (writeFile manifestPath) . BS.unpack . encode

-- | Get the app's Component.IO ID
appId :: AppInfo -> String
appId appInfo = parseRepoInfo $ splitDirectories $ repo appInfo

-- | Parse the repo info into an app ID
parseRepoInfo :: [String] -> String
parseRepoInfo (owner : appRepo : _) = owner ++ "-" ++ appRepo
parseRepoInfo _ = ""

-- | Given a possible component ID, return the user and the repo
-- constituents
parseComponentId :: String -> Maybe (String, String)
parseComponentId cId
  | repoL > 0 = Just ((T.unpack user), (T.unpack repo))
  | otherwise = Nothing
  where
    (user, repo) = breakOn (T.pack "-") (T.pack cId)
    repoL = length $ T.unpack repo
