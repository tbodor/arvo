module ParserSpec (spec) where

import Data.Void (Void)
import Test.Hspec
import Test.Hspec.Megaparsec
-- import Test.QuickCheck

import Text.Megaparsec (ParseErrorBundle)

import qualified Parser
import Syntax


spec :: Spec
spec = do
  describe "parse variable" $ do
    it "single char" $ parse "x" `shouldParse` Variable "x"
    it "multi char" $ parse "abc" `shouldParse` Variable "abc"

  describe "parse lambda" $ do
    it "identity" $ parse "λx.x" `shouldParse` Lambda "x" (Variable "x")
    it "omega" $
      parse "(λx.x x) (λx.x x)" `shouldParse`
        Application (Lambda "x" $ Application (Variable "x") (Variable "x"))
                    (Lambda "x" $ Application (Variable "x") (Variable "x"))
    it "misc terms" $ do
      verify "λx.λy.λz.x z (y z)" "λx.λy.λz.x z (y z)"
      verify "(λf.λx.f (f x))" "λf.λx.f (f x)" 
      verify "(λf.λx.f (f x)) a" "(λf.λx.f (f x)) a"
      verify "(λf.λx.f (f x)) (λf.λx.f (f x)) (λx.x) (λx.x)" "(λf.λx.f (f x)) (λf.λx.f (f x)) (λx.x) (λx.x)"

  describe "parse application" $ do
    it "a b" $ parse "a b" `shouldParse` Application (Variable "a") (Variable "b")
    it "(a b)" $ parse "(a b)" `shouldParse` Application (Variable "a") (Variable "b")
    it "a b c" $ parse "a b c" `shouldParse` Application (Application (Variable "a") (Variable "b")) (Variable "c")
    it "(a b c)" $ parse "(a b c)" `shouldParse` Application (Application (Variable "a") (Variable "b")) (Variable "c")


    -- it "returns the first element of an *arbitrary* list" $
      -- property $ \x xs -> head (x:xs) == (x :: Int)

    -- it "throws an exception if used with an empty list" $ do
      -- evaluate (head []) `shouldThrow` anyException


verify :: String -> String -> Expectation
verify expression expected = parse expression `parseSatisfies` (\t -> show t == expected)


parse :: String -> Either (ParseErrorBundle String Void) Term
parse = Parser.rawParse
