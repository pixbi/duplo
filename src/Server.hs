import Network.Wai (pathInfo)
import Network.HTTP.Types (status200)
import Control.Monad.Trans (liftIO)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Web.Scotty
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Data.List (intercalate)
import Control.Lens
import System.FilePath.Posix (takeExtension)

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
      path      <- fmap ((intercalate "/") . (fmap T.unpack) . pathInfo) request
      -- Setting root
      let path'  = "public/" ++ path
      exists    <- liftIO $ doesFileExist path'

      status status200

      if   exists
      then normalFile path'
      else defaultFile

normalFile :: FilePath -> ActionM ()
normalFile path = do
    let contentType = guessType path
    file path
    setHeader "Content-Type" $ LT.pack contentType

defaultFile :: ActionM ()
defaultFile = do
    file "public/index.html"
    setHeader "Content-Type" "text/html"

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
