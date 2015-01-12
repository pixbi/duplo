{-# LANGUAGE DeriveGeneric #-}

module Development.Duplo.Types.AppInfo where

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.HashMap.Strict (HashMap, empty)
import           GHC.Generics        (Generic)

type Dependencies = HashMap String String
type Modes = HashMap String [String]

-- | App information extracted from `component.json`
data AppInfo = AppInfo
               { name         :: String
               , repo         :: String
               , version      :: String
               , dependencies :: Dependencies
               , modes        :: Maybe Modes
               , images       :: [String]
               , scripts      :: [String]
               , styles       :: [String]
               , templates    :: [String]
               , fonts        :: [String]
               } deriving
               ( Show
               , Generic
               )

defaultAppInfo = AppInfo
               { name         = ""
               , repo         = ""
               , version      = ""
               , dependencies = empty
               , modes        = Nothing
               , images       = []
               , scripts      = []
               , styles       = []
               , templates    = []
               , fonts        = []
               }

instance FromJSON AppInfo
instance ToJSON AppInfo
