{-# LANGUAGE TupleSections #-}

module Development.Duplo.Component where

import           Control.Applicative                         ((<$>), (<*>))
import           Control.Exception                           (throw)
import           Control.Monad                               (liftM, when)
import           Control.Monad.Trans.Class                   (lift)
import           Data.Aeson                                  (decode, encode)
import           Data.Aeson.Encode.Pretty                    (encodePretty)
import           Data.ByteString.Lazy.Char8                  (ByteString)
import qualified Data.ByteString.Lazy.Char8                  as BS (pack,
                                                                    unpack)
import           Data.HashMap.Lazy                           (empty, keys,
                                                              lookup)
import           Data.List                                   (isPrefixOf)
import           Data.Map                                    (fromList)
import           Data.Maybe                                  (fromJust,
                                                              fromMaybe)
import           Data.Text                                   (breakOn)
import qualified Data.Text                                   as T (pack, unpack)
import           Development.Duplo.Types.AppInfo             (AppInfo (..))
import qualified Development.Duplo.Types.AppInfo             as AI
import qualified Development.Duplo.Types.Builder             as BD
import           Development.Shake                           hiding (doesDirectoryExist,
                                                              doesFileExist, getDirectoryContents)
import           Development.Shake.FilePath                  ((</>))
import           Prelude                                     hiding (lookup)
import           System.Directory                            (doesDirectoryExist,
                                                              doesFileExist, getCurrentDirectory, getDirectoryContents)
import           System.FilePath.FilePather.FilterPredicate  (filterPredicate)
import           System.FilePath.FilePather.Find             (findp)
import           System.FilePath.FilePather.RecursePredicate (recursePredicate)
import           System.FilePath.Posix                       (dropExtension, dropTrailingPathSeparator,
                                                              equalFilePath,
                                                              makeRelative,
                                                              splitDirectories,
                                                              takeFileName)

type Version = (String, String)

-- | Each application must have a `component.json`
manifestName = "component.json"

readManifest :: IO AppInfo
readManifest = do
    exists <- doesFileExist manifestName

    if   exists
    then readManifest' manifestName
    else throw $ BD.MissingManifestException manifestName

readManifest' :: FilePath -> IO AppInfo
readManifest' path = do
    manifest <- readFile path
    let maybeAppInfo = decode (BS.pack manifest) :: Maybe AppInfo

    case maybeAppInfo of
      Nothing -> throw $ BD.MalformedManifestException path
      Just a  -> return a

writeManifest :: AppInfo -> IO ()
writeManifest = writeFile manifestName . BS.unpack . encodePretty

-- | Get the app's Component.IO ID
appId :: AppInfo -> String
appId appInfo = parseRepoInfo $ splitDirectories $ AI.repo appInfo

-- | Parse the repo info into an app ID
parseRepoInfo :: [String] -> String
parseRepoInfo (owner : appRepo : _) = owner ++ "-" ++ appRepo
parseRepoInfo _ = ""

-- | Given a possible component ID, return the user and the repo
-- constituents
parseComponentId :: String -> Either String (String, String)
parseComponentId cId
  | repoL > 0 = Right (T.unpack user, T.unpack repo)
  | otherwise = Left $ "No component ID found with " ++ cId
  where
    (user, repo) = breakOn (T.pack "-") (T.pack cId)
    repoL = length $ T.unpack repo

-- | Given a path, find all the `component.json` and return a JSON string
extractCompVersions :: FilePath -> IO String
extractCompVersions path = do
    -- Get all the relevant paths
    paths <- getAllManifestPaths path
    -- Construct the pipeline
    let toVersion path = appInfoToVersion . decodeManifest path . BS.pack
    let takeVersion path = liftM (toVersion path) (readFile path)
    -- Go through it
    manifests <- mapM takeVersion paths
    -- Marshalling
    return $ BS.unpack $ encode $ fromList manifests

-- | Given a path and the file content that the path points to, return the
-- manifest in `AppInfo` form.
decodeManifest :: FilePath -> ByteString -> AppInfo
decodeManifest path content = fromMaybe whenNothing decodedContent
  where
    whenNothing = throw $ BD.MalformedManifestException path
    decodedContent = decode content :: Maybe AppInfo

appInfoToVersion :: AppInfo -> Version
appInfoToVersion appInfo = (AI.name appInfo, AI.version appInfo)

-- | Given a path, find all the `component.json`s
getAllManifestPaths :: FilePath -> IO [FilePath]
getAllManifestPaths root = allPaths
  where
    -- Filter for `component.json`.
    matchName path t = takeFileName path == takeFileName manifestName
    filterP          = filterPredicate matchName
    -- We only care about the root directory and the `components/` directory.
    componentsPath   = dropTrailingPathSeparator $ root </> "components/"
    proceed absPath  = equalFilePath root absPath
                    || componentsPath `isPrefixOf` absPath
    recurseP         = recursePredicate proceed
    -- Collect the paths here.
    allPaths         = findp filterP recurseP root

-- | Get the component dependency list by providing a mode, or not.
getDependencies :: Maybe String -> IO [FilePath]
-- | Simply get all dependencies if no mode is provided.
getDependencies Nothing = do
    cwd <- getCurrentDirectory
    let depDir = cwd </> "components/"
    depDirExists <- doesDirectoryExist depDir
    let filterRegular = fmap $ filter isRegularFile

    filterRegular $
      if   depDirExists
      then getDirectoryContents depDir
      else return []
-- | Only select the named dependencies.
getDependencies (Just mode) = do
    fullDeps <- fmap AI.dependencies readManifest
    depModes <- fmap AI.modes readManifest
    getDependencies' fullDeps $ case depModes of
                                  Just d -> lookup mode d
                                  Nothing -> Nothing

-- | Helper function to get the selected dependency list given the full
-- dependency list, all modes, and the target mode to select the list by.
getDependencies' :: AI.Dependencies -> Maybe [String] -> IO [FilePath]
-- If somehow there isn't a mode defined, switch over to `Nothing`.
getDependencies' deps Nothing = getDependencies Nothing
-- If there is something, fetch only those dependencies.
getDependencies' deps (Just modeDeps) = return modeDeps

-- | Regular file != *nix-style hidden file
isRegularFile :: FilePath -> Bool
isRegularFile = not . isPrefixOf "."
