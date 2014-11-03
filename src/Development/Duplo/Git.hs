module Development.Duplo.Git
  ( commit
  , Level
  ) where

import Development.Shake
import Development.Shake.FilePath ((</>))
import Development.Duplo.Utilities (logAction)
import qualified Development.Duplo.ComponentIO as I
import Control.Lens hiding (Action, Level)
import qualified Development.Duplo.Config as C
import Data.List (intercalate)
import Data.Text (unpack, pack, splitOn)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import Debug.Trace (trace)

type Level      = String
type Version    = String
type Subversion = String

-- We use Semantic Versioning protocol
versionLength :: Int
versionLength = 3

-- | Commit to git and bump version for current project
commit :: C.BuildConfig
       -> Level
       -> Action ()
commit config level = do
    let utilPath = config ^. C.utilPath
    manifest <- liftIO I.readManifest
    let version = I.version manifest

    -- First stash any outstanding change
    command_ [] "git" ["stash"]
    -- Then make sure we're on master
    command_ [] "git" ["checkout", "master"]

    -- Increment version according to level
    let newVersion = incrementVersion level version
    -- Update manifest
    let newManifest = updateVersion manifest newVersion
    -- Commit manifest
    liftIO $ I.writeManifest newManifest

    -- Commit with the version
    command_ [] (utilPath </> "commit.sh") [newVersion]

    logAction $ "Bumped version from " ++ version ++ " to " ++ newVersion

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

updateVersion :: I.AppInfo -> Version -> I.AppInfo
updateVersion manifest version = manifest { I.version = version }
