{-import Network.Wai (pathInfo)-}
{-import Network.HTTP.Types (status200)-}
{-import Control.Monad.Trans (liftIO)-}
{-import System.Directory (doesFileExist)-}
{-import System.Environment (getArgs)-}
{-import Web.Scotty-}
{-import Data.Text (Text)-}
{-import qualified Data.Text as T-}
{-import Data.Text.Lazy (Text)-}
{-import qualified Data.Text.Lazy as LT-}
{-import Data.List (intercalate)-}
{-import Control.Lens-}
{-import System.FilePath.Posix (takeExtension)-}
{-import Development.Duplo.Server (serve)-}

{-main = do-}
{-    -- Command-line arguments-}
{-    args <- getArgs-}
{-    let port = read $ args ^. element 0 :: Int-}

{-    serve port-}
