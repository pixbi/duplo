{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

module Development.Duplo.Types.AppInfo where

import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict (HashMap, empty)
import GHC.Generics (Generic)

-- | App information extracted from `component.json`
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

instance FromJSON AppInfo
instance ToJSON AppInfo
