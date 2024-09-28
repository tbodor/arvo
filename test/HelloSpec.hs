module HelloSpec (spec) where

import Hello

import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec = do
  describe "hello" $ do
    it "says hello world" $ hello "world" `shouldBe` "Hello world"

    it "says hello to arbitrary name" $
      property $ \n -> hello n == "Hello " ++ n
