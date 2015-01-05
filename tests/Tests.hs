module Main where

import qualified Development.Duplo.Component as DC
import Development.Duplo.Types.AppInfo
import Test.Hspec

main :: IO ()
main = hspec $
  describe "Component support" $
    it "extracts component ID" $ do
      let appInfo = defaultAppInfo { repo = "pixbi/duplo" }
      let appId   = DC.appId appInfo
      appId `shouldBe` "pixbi-duplo"
