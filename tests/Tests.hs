{-# LANGUAGE TemplateHaskell #-}

module Main where

import Development.Duplo.Types.AppInfo
import Test.Hspec
import Test.QuickCheck
import qualified Development.Duplo.Component as DC
import Data.Functor (fmap)

runTests = $verboseCheckAll

main :: IO ()
main = do
  -- First QuickCheck
  verboseCheck prop_slashToDash

  -- Then HSpec
  hspec $
    describe "Component support" $ do
      it "extracts component ID" $ do
        let appInfo = defaultAppInfo { repo = "pixbi/duplo" }
        let appId   = DC.appId appInfo
        appId `shouldBe` "pixbi-duplo"

  return ()

newtype NameString = NameString
                     { unwrapNameString :: String
                     } deriving Show

instance Arbitrary NameString where
    arbitrary = fmap NameString $ listOf $ elements $ ['a'..'z']
                                                    ++ ['A'..'Z']
                                                    ++ ['/', '-']

instance Arbitrary AppInfo where
    arbitrary = do
      nameString  <- arbitrary :: Gen NameString
      let string = unwrapNameString nameString
      return defaultAppInfo { repo = string }

prop_slashToDash appInfo =
    (not (null repo'))   ==>
    (head repo' /= '/')  ==>
    (last repo' /= '/')  ==>
    (onlyOneSlash repo') ==>
      value == replaceFirst '/' '-' repo'
  where
    value        = DC.appId appInfo
    repo'        = repo appInfo
    onlyOneSlash = (== 1) . length . filter (== '/')

-- | Replace the first occurence of a character in a string.
replaceFirst :: Char -> Char -> String -> String
replaceFirst needle replacement haystack
    | null rest = haystack
    | otherwise = match ++ [replacement] ++ tail rest
  where
    (match, rest) = break (== needle) haystack
