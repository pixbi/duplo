{-# LANGUAGE DeriveDataTypeable #-}

module Development.Duplo.Types.Builder where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

data BuilderException = MissingGithubUserException
                      | MissingGithubRepoException
  deriving (Typeable)

instance Exception BuilderException

instance Show BuilderException where
    show MissingGithubUserException =
      "There must be a GitHub user."
    show MissingGithubRepoException =
      "There must be a GitHub repo."
