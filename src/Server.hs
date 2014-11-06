{-# LANGUAGE OverloadedStrings #-}

import Network.Wai (pathInfo)
import Network.HTTP.Types (status200)
import Control.Monad.Trans (liftIO)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Web.Scotty
import Data.Text (Text, unpack)
import Data.Text.Lazy (Text, pack)
import Data.List (intercalate)
import Control.Lens

type Port = Int

main = do
    -- Command-line arguments
    args     <- getArgs
    let port  = read $ args ^. element 0 :: Int

    putStrLn $ "Server started on port " ++ show port
    scotty port serve

serve :: ScottyM ()
serve = do
    -- Always match because we're checking for files, not routes
    notFound $ do
      path     <- fmap ((intercalate "/") . (fmap unpack) . pathInfo) request
      -- Setting root
      let path' = "public/" ++ path
      exists   <- liftIO $ doesFileExist path'

      status status200

      if   exists
      then file path'
      else file "public/index.html"
