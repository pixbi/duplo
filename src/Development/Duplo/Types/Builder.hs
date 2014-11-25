{-# LANGUAGE DeriveDataTypeable #-}

module Development.Duplo.Types.Builder where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

data BuilderException = MissingGithubUserException
                      | MissingGithubRepoException
                      | MalformedManifestException String
                      | MissingManifestException String
  deriving (Typeable)

instance Exception BuilderException

instance Show BuilderException where
    show MissingGithubUserException =
      "There must be a GitHub user."
    show MissingGithubRepoException =
      "There must be a GitHub repo."
    show (MalformedManifestException path) =
      "The manifest file `" ++ path ++ "` is not a valid duplo JSON."
    show (MissingManifestException path) =
      "`" ++ path ++ "` is expected at the current location."
