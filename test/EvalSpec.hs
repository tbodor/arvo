module EvalSpec (spec) where

import Interpreter
import qualified Parser
import Syntax (Term)
import Test.Hspec

spec :: Spec
spec = do
  describe "eval" $ do
    it "IK" $ normalise definitions (parse "I K") `shouldBe` normalise [] (parse "λx.λy.x")
    it "KI" $ normalise definitions (parse "K I") `shouldBe` normalise [] (parse "λy.λx.x")
    it "SKK" $ normalise definitions (parse "S K K") `shouldBe` normalise [] (parse "λz.z")
    it "misc" $ normalise [] (parse "(λf.λx.f (f x)) (λf.λx.f (f x)) (λx.x) (λx.x)") `shouldBe` parse "λx.x"

definitions :: Environment Value
definitions = map ev
    [ ("S", parse "λx.λy.λz.x z (y z)")
    , ("K", parse "λx.λy.x")
    , ("I", parse "λx.x")
    ]
  where
    ev (name, term) = (name, eval [] term)

parse :: String -> Term
parse s = case Parser.parse s of
  Left err -> error (show err)
  Right term -> term
