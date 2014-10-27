module Development.Duplo.ComponentIO
  ( appName
  , appVersion
  , appRepo
  ) where

import Control.Applicative ((<$>), (<*>), empty)
import Development.Shake hiding (readFile)
import Data.Aeson ((.:))
import qualified Data.Aeson as AES
import Data.Text hiding (empty)
import qualified Data.ByteString.Lazy.Char8 as BSL

-- | Each application must have a `component.json`
manifestPath :: String
manifestPath = "component.json"

data AppInfo = AppInfo {
  name    :: String,
  repo    :: String,
  version :: String
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
