module Main where

import qualified Development.Duplo.Component as DC
import Development.Duplo.Types.AppInfo
import Test.Hspec

main :: IO ()
main = hspec $
  describe "Component support" $
    it "extracts component ID" $ do
      let appInfo = AppInfo { repo = "pixbi/duplo" }
      let id      = DC.appId appInfo
      id `shouldBe` "pixbi-duplo"
