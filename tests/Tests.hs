{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import qualified Development.Duplo.Component     as DC
import           Development.Duplo.Types.AppInfo
import           Test.HUnit                      ()
import           Test.QuickCheck                 ()

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck           as QC

main :: IO ()
main = defaultMain tests

setMaxSuccess :: TestTree -> TestTree
setMaxSuccess = localOption $ QuickCheckTests 5000

tests :: TestTree
tests = setMaxSuccess $ testGroup "Tests"
  [ testGroup "Component support"
    [ QC.testProperty "slash is transformed into dash" $ \appInfo ->
        let
          value        = DC.appId appInfo
          repo'        = repo appInfo
          onlyOneSlash = (== 1) . length . filter (== '/')
        in
          not (null repo')    ==>
          (head repo' /= '/') ==>
          (last repo' /= '/') ==>
          onlyOneSlash repo'  ==>
            value == replaceFirst '/' '-' repo'

    , testCase "extracts component ID" $ do
      let appInfo = defaultAppInfo { repo = "pixbi/duplo" }
      let appId   = DC.appId appInfo
      appId @?= "pixbi-duplo"

    ]
  ]


newtype NameString = NameString
                     { unwrapNameString :: String
                     } deriving Show

instance Arbitrary NameString where
    arbitrary = fmap NameString $ listOf $ elements $ ['a'..'z']
                                                    ++ ['A'..'Z']
                                                    ++ "/-"

instance Arbitrary AppInfo where
    arbitrary = do
      nameString  <- arbitrary :: Gen NameString
      let string = unwrapNameString nameString
      return defaultAppInfo { repo = string }

-- | Replace the first occurence of a character in a string.
replaceFirst :: Char -> Char -> String -> String
replaceFirst needle replacement haystack
    | null rest = haystack
    | otherwise = match ++ [replacement] ++ tail rest
  where
    (match, rest) = break (== needle) haystack
