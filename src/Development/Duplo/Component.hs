{-# LANGUAGE TupleSections #-}

module Development.Duplo.Component where

import           Control.Exception               (throw)
import           Control.Lens.Operators
import           Control.Monad                   (liftM, unless)
import           Data.Aeson                      (decode, encode)
import           Data.Aeson.Encode.Pretty        (encodePretty)
import           Data.ByteString.Lazy.Char8      (ByteString)
import qualified Data.ByteString.Lazy.Char8      as BS (pack, unpack)
import           Data.HashMap.Lazy               (lookup)
import           Data.List                       (isPrefixOf)
import           Data.Map                        (fromList)
import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (breakOn)
import qualified Data.Text                       as T (pack, unpack)
import           Development.Duplo.Types.AppInfo (AppInfo (..))
import qualified Development.Duplo.Types.AppInfo as AI
import qualified Development.Duplo.Types.Builder as BD
import qualified Development.Duplo.Types.Config  as TC
import           Development.Shake               hiding (doesDirectoryExist,
                                                  doesFileExist,
                                                  getDirectoryContents)
import           Development.Shake.FilePath      ((</>))
import           Prelude                         hiding (lookup)
import           System.Directory                (doesDirectoryExist,
                                                  doesFileExist,
                                                  getCurrentDirectory,
                                                  getDirectoryContents)
import           System.FilePath.Posix           (splitDirectories)

type Version = (String, String)

-- | Each application must have a `component.json`
manifestName :: FilePath
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
  | repoL > 0 = Right (T.unpack user, T.unpack userRepo)
  | otherwise = Left $ "No component ID found with " ++ cId
  where
    (user, userRepo) = breakOn (T.pack "-") (T.pack cId)
    repoL = length $ T.unpack userRepo

-- | Given a path, find all the `component.json` and return a JSON string
extractCompVersions :: TC.BuildConfig -> Action String
extractCompVersions config = do
    let path = config ^. TC.cwd
    -- Get all the relevant paths
    paths <- getAllManifestPaths config path
    -- Construct the pipeline
    let toVersion path'   = appInfoToVersion . decodeManifest path' . BS.pack
    let takeVersion path' = liftM (toVersion path') (readFile path')
    -- Go through it
    manifests <- mapM (liftIO . takeVersion) (paths ++ ["./component.json"])
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
getAllManifestPaths :: TC.BuildConfig -> FilePath -> Action [FilePath]
getAllManifestPaths config root = do
    let cwd      = config ^. TC.cwd
    let utilPath = config ^. TC.utilPath
    let depsPath = config ^. TC.depsPath
    -- Find all dependencies' manifests
    Stdout out <- command [] (utilPath </> "find.sh") [depsPath, manifestName]
    -- Get the current repo's manifest
    let currentRepoManifest = cwd </> "component.json"
    -- These should be all `component.json`s
    return $ lines out ++ [currentRepoManifest]

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
getDependencies' _ Nothing = getDependencies Nothing
-- If there is something, fetch only those dependencies.
getDependencies' _ (Just modeDeps) = return modeDeps

-- | Regular file != *nix-style hidden file
isRegularFile :: FilePath -> Bool
isRegularFile = not . isPrefixOf "."
