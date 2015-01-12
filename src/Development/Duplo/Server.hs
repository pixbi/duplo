module Development.Duplo.Server where

import           Control.Monad.Trans      (liftIO)
import           Data.List                (intercalate)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Lazy           (Text)
import qualified Data.Text.Lazy           as LT
import           Network.HTTP.Types       (status200)
import           Network.Wai              (pathInfo)
import           Network.Wai.Handler.Warp (Port)
import           System.Directory         (doesFileExist)
import           System.Environment       (getArgs)
import           System.FilePath.Posix    (takeExtension)
import           Web.Scotty

serve :: Port -> IO ()
serve port = do
    putStrLn $ "\n>> Starting server on port " ++ show port
    scotty port serve'

serve' :: ScottyM ()
serve' =
    -- Always match because we're checking for files, not routes
    notFound $ do
      path <- fmap (intercalate "/" . fmap T.unpack . pathInfo) request
      -- Setting root
      let path' = "public/" ++ path
      exists <- liftIO $ doesFileExist path'

      status status200

      if   exists
      then normalFile path'
      else returnDefault path'

normalFile :: FilePath -> ActionM ()
normalFile path = do
    let contentType = guessType path
    file path
    setHeader "Content-Type" $ LT.pack contentType

-- | Return a default file depending on file type. Return the default HTML
-- file otherwise.
returnDefault :: FilePath -> ActionM ()
returnDefault path = do
    let extension = takeExtension path

    file $ case extension of
             ".css" -> "public/index.css"
             ".js"  -> "public/index.js"
             _      -> "public/index.html"

    setHeader "Content-Type" $ case extension of
                                 ".css" -> "text/css"
                                 ".js"  -> "text/javascript"
                                 _      -> "text/html"

-- | "Guess" the content type from the path's file type
guessType :: String -> String
guessType path = case takeExtension path of
                   ".htm" -> "text/html"
                   ".html" -> "text/html"
                   ".css" -> "text/css"
                   ".js" -> "application/x-javascript"
                   ".png" -> "image/png"
                   ".jpeg" -> "image/jpeg"
                   ".jpg" -> "image/jpeg"
                   ".woff" -> "application/font-woff"
                   ".ttf" -> "application/font-ttf"
                   ".eot" -> "application/vnd.ms-fontobject"
                   ".otf" -> "application/font-otf"
                   ".svg" -> "image/svg+xml"
                   otherwise -> "application/octet-stream"
