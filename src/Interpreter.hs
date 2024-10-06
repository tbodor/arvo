module Interpreter
  ( normalise
  , eval
  , Environment
  , Value (..)
  )
where

import Environment
import Syntax (Name, Term (..))
import Prelude hiding (lookup)


data Value
  = Closure (Environment Value) Name Term
  | Neutral Neutral
  deriving (Show, Eq)

data Neutral
  = NVariable Name
  | NApplication Neutral Value
  deriving (Show, Eq)

eval :: Environment Value -> Term -> Value
eval env (Variable name) = case lookup name env of
  Nothing -> error ("Undefined variable " ++ name)
  Just v -> v
eval env (Lambda parameter body) = Closure env parameter body
eval env (Application function argument) = apply functionValue argumentValue
  where
    functionValue = eval env function
    argumentValue = eval env argument

apply :: Value -> Value -> Value
apply (Closure staticEnv parameter body) argument = eval localEnv body
  where
    localEnv = extend parameter argument staticEnv
apply (Neutral n) argument = Neutral $ NApplication n argument


fresh :: [Name] -> Name -> Name
fresh names n
    | n `elem` names = fresh names (n ++ "'")
    | otherwise = n

uneval :: [Name] -> Value -> Term
uneval names (Closure e x b) = Lambda x' $ uneval (x':names) evaluatedBody
    where
      x' = fresh names x
      env' = (x, Neutral (NVariable x')) : e
      evaluatedBody = eval env' b

uneval names (Neutral n) = unevalNeutral names n

unevalNeutral :: [Name] -> Neutral -> Term
unevalNeutral _ (NVariable x) = Variable x
unevalNeutral names (NApplication f x) = Application (unevalNeutral names f) (uneval names x)


-- Note: no alpha equivalence (no renaming to a canonical form, SKK=I, but it will return \z.z, not \x.x)
normalise :: Environment Value -> Term -> Term
normalise e = uneval [] . eval e
