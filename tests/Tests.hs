module Main where

import Development.Duplo.Types.AppInfo
import Test.Hspec
import qualified Development.Duplo.Component as DC

main :: IO ()
main = hspec $
  describe "Component support" $
    it "extracts component ID" $ do
      let appInfo = defaultAppInfo { repo = "pixbi/duplo" }
      let appId   = DC.appId appInfo
      appId `shouldBe` "pixbi-duplo"
