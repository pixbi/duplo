{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

module Development.Duplo.Types.Version
  ( Version(..)
  ) where

import Data.Aeson (FromJSON, ToJSON(..), object, (.=))
import GHC.Generics (Generic)

-- | AST for component versions
data Version  = Version { name    :: String
                        , version :: String
                        } deriving (Show, Generic)

instance ToJSON Version where
    toJSON (Version name version) = object [ "name"    .= name
                                           , "version" .= version
                                           ]
