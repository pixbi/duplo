module Development.Duplo.ComponentIO
  ( appName
  , appVersion
  , appRepo
  , appId
  , parseComponentId
  ) where

import Control.Applicative ((<$>), (<*>), empty)
import Development.Shake hiding (readFile)
import Data.Aeson ((.:))
import qualified Data.Aeson as AES
import Data.Text (breakOn, unpack, pack)
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.FilePath.Posix (splitDirectories)

-- | Each application must have a `component.json`
manifestPath :: String
manifestPath = "component.json"

data AppInfo = AppInfo { name    :: String
                       , repo    :: String
                       , version :: String
                       } deriving (Show)

-- | Instance for Aeson
instance AES.FromJSON AppInfo where
  parseJSON (AES.Object obj) = AppInfo
                           <$> obj .: pack "name"
                           <*> obj .: pack "repo"
                           <*> obj .: pack "version"
  parseJSON _                = empty

-- | Get application info
appInfo :: IO AppInfo
appInfo = do
  manifest <- readFile manifestPath
  let parsed = AES.decode (BSL.pack manifest) :: Maybe AppInfo
  case parsed of
    Just info -> return info
    -- TODO: This needs to be handled as an exception
    Nothing   -> return AppInfo {name = "", repo = "", version = ""}

----------
-- Accessors

appName :: IO String
appName = fmap name appInfo

appVersion :: IO String
appVersion = fmap version appInfo

appRepo :: IO String
appRepo = fmap repo appInfo

appId :: IO String
appId = do
  appRepo <- fmap repo appInfo
  let (user : repo : _) = splitDirectories appRepo
  return $ user ++ "-" ++ repo

-- | Given a possible component ID, return the user and the repo
-- constituents
parseComponentId :: String -> Maybe (String, String)
parseComponentId id
  | repoL > 0 = Just ((unpack user), (unpack repo))
  | otherwise = Nothing
  where
    (user, repo) = breakOn (pack "-") (pack id)
    repoL = length $ unpack repo
