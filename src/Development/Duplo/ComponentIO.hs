{-# LANGUAGE TemplateHaskell #-}

module Development.Duplo.ComponentIO
  ( appId
  , parseComponentId
  , readManifest
  , writeManifest
  , extractCompVersions
  ) where

import Control.Applicative ((<$>), (<*>))
import Development.Shake hiding (doesFileExist)
import Data.Text (breakOn)
import qualified Data.Text as T (unpack, pack)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS (unpack, pack)
import System.FilePath.Posix (splitDirectories)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Class (lift)
import System.Directory (doesFileExist)
import Development.Duplo.Types.AppInfo (AppInfo(..))
import qualified Development.Duplo.Types.AppInfo as AI
import Development.Duplo.Types.Version (Version(..))
import qualified Development.Duplo.Types.Version as VS
import Data.Aeson (encode, decode)
import Data.Maybe (fromJust)
import System.FilePath.FilePather.Find (findp)
import System.FilePath.FilePather.FilePathPredicate (always)
import System.FilePath.FilePather.FilterPredicate (filterPredicate)
import System.FilePath.FilePather.RecursePredicate (recursePredicate)
import System.FilePath.Posix (takeFileName)

-- | Each application must have a `component.json`
manifestName = "component.json"

readManifest :: MaybeT IO AppInfo
readManifest = do
    exists <- liftIO $ doesFileExist manifestName

    if   exists
    then readManifest' manifestName
    else MaybeT $ return Nothing

readManifest' :: FilePath -> MaybeT IO AppInfo
readManifest' path = do
    manifest <- liftIO $ readFile path
    let maybeAppInfo = decode (BS.pack manifest) :: Maybe AppInfo

    case maybeAppInfo of
      Nothing -> MaybeT $ return Nothing
      Just a  -> MaybeT $ return $ Just a

writeManifest :: AppInfo -> IO ()
writeManifest = (writeFile manifestName) . BS.unpack . encode

-- | Get the app's Component.IO ID
appId :: AppInfo -> String
appId appInfo = parseRepoInfo $ splitDirectories $ AI.repo appInfo

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

-- | Given a path, find all the `component.json` and return a JSON string
extractCompVersions :: FilePath -> IO String
extractCompVersions path = do
    paths     <- getAllManifestPaths path
    manifests <- mapM ((fmap BS.pack) . readFile) paths
    let decodeManifest = \ x -> fromJust $ (decode x :: Maybe AppInfo)
    let manifests' = fmap (appInfoToVersion . decodeManifest) manifests
    return $ BS.unpack $ encode manifests'

appInfoToVersion :: AppInfo -> Version
appInfoToVersion appInfo = Version (AI.name appInfo) (AI.version appInfo)

-- | Given a path, find all the `component.json`s
getAllManifestPaths :: FilePath -> IO [FilePath]
getAllManifestPaths path =
    findp filterP always path
  where
    filterP   = filterPredicate matchName
    matchName = \ path t -> takeFileName path == takeFileName manifestName
