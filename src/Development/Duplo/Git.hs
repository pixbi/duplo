module Development.Duplo.Git where

import Control.Applicative ((<$>))
import Control.Lens hiding (Action, Level)
import Control.Monad.Except (runExceptT)
import Data.List (intercalate, filter)
import Data.Text (unpack, pack, splitOn)
import Development.Shake
import Development.Shake.FilePath ((</>))
import System.FilePath.Posix (makeRelative)
import qualified Development.Duplo.Component as CM
import qualified Development.Duplo.Types.AppInfo as AI
import qualified Development.Duplo.Types.Config as TC

type Level         = String
type Version       = String
type Subversion    = String
type FileExtension = String

-- We use Semantic Versioning protocol
versionLength :: Int
versionLength = 3

-- | Commit to git and bump version for current project
commit :: TC.BuildConfig
       -> Level
       -> Action (Version, Version)
commit config level = do
    let utilPath = config ^. TC.utilPath
    appInfo <- liftIO CM.readManifest
    let version = AI.version appInfo
    let cwd = config ^. TC.cwd
    let manifest = cwd </> "component.json"

    -- First stash any outstanding change
    command_ [] "git" ["stash"]
    -- Then make sure we're on master
    command_ [] "git" ["checkout", "master"]

    -- Increment version according to level
    let newVersion = incrementVersion level version
    -- Update app info
    let appInfo' = updateVersion appInfo newVersion
    -- Update registered file list with Component.IO
    appInfo'' <- updateFileRegistry config appInfo'
    -- Commit app info
    liftIO $ CM.writeManifest appInfo''

    -- Commit with the version
    command_ [] (utilPath </> "commit.sh") [newVersion]

    return (version, newVersion)

-- | Bump a version, given a bump type
incrementVersion :: Version
                 -> Level
                 -> Version
incrementVersion level version =
    intercalate "." $ incrementSubversion expanded index
  where
    expanded = fmap unpack $ splitOn (pack ".") (pack version)
    index    = case level of
                 "major" -> 0
                 "minor" -> 1
                 "patch" -> 2

-- | Given a version component, increment it.
incrementSubversion :: [Subversion] -> Int -> [Subversion]
incrementSubversion version index =
    resetVer
  where
    oldPart  = version ^. (element index)
    oldPart' = read oldPart :: Int
    newPart  = oldPart' + 1
    newPart' = show newPart :: String
    -- Increment target subversion
    incrVer  = version & (element index) .~ newPart'
    -- Reset other
    resetVer = resetSubversion incrVer (index + 1) versionLength

-- | Reset a version component. The reset cascades up to the max subversion
-- index.
resetSubversion :: [Subversion] -> Int -> Int -> [Subversion]
resetSubversion version index max
  | index <= max = let newVersion = resetSubversion version (index + 1) max
                   in  newVersion & (element index) .~ "0"
  | otherwise    = version

updateVersion :: AI.AppInfo -> Version -> AI.AppInfo
updateVersion manifest version = manifest { AI.version = version }

-- | Read from the given directory and update the app manifest object.
updateFileRegistry :: TC.BuildConfig -> AI.AppInfo -> Action AI.AppInfo
updateFileRegistry config appInfo = do
    let cwd = config ^. TC.cwd
    let utilPath = config ^. TC.utilPath
    let appPath = cwd </> "app"
    let assetPath = appPath </> "assets"
    let imagePath = assetPath </> "images"
    let fontPath = assetPath </> "fonts"

    -- Helper functions
    let find = \path pttrn -> command [] (utilPath </> "find.sh") [path, pttrn]
    let split = (fmap unpack) . (splitOn "\n") . pack
    let makeRelative' = makeRelative cwd
    let filterNames = filter ((> 0) . length)
    let prepareFileList = filterNames . (fmap $ makeRelative cwd) . split

    -- Collect eligible files
    Stdout scripts <- find appPath "*.js"
    Stdout styles <- find appPath "*.styl"
    Stdout markups <- find appPath "*.jade"
    Stdout images <- find imagePath "*"
    Stdout fonts <- find fontPath "*"

    -- Convert to arrays and make relative to cwd
    let scripts' = prepareFileList scripts
    let styles' = prepareFileList styles
    let markups' = prepareFileList markups
    let images' = prepareFileList images
    let fonts' = prepareFileList fonts

    -- Update manifest
    return appInfo { AI.images    = images'
                   , AI.scripts   = scripts'
                   , AI.styles    = styles'
                   , AI.templates = markups'
                   , AI.fonts     = fonts'
                   }
