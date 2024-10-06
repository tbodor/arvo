module Eval
  ( eval
  , Environment
  , Value (..)
  )
where

import Syntax (Name, Term (..))

type Environment a = [(Name, a)]

data Value
  = Closure (Environment Value) Name Term
  deriving (Show, Eq)

-- | Evaluate to weak head normal form (doesn't evaluate under the lambda)
-- 
-- The eval function is partial: it will fail on unbound variables and will not terminate for some terms (e.g. Ω).
eval :: Environment Value -> Term -> Value
eval env (Variable name) = case lookup name env of
  Nothing -> error ("Undefined variable " ++ name)  -- Should have better error handling
  Just v -> v
eval env (Lambda parameter body) = Closure env parameter body
eval env (Application function argument) = apply functionValue argumentValue
  where
    functionValue = eval env function
    argumentValue = eval env argument

apply :: Value -> Value -> Value
apply (Closure staticEnv parameter body) argument = eval localEnv body
  where
    localEnv = (parameter, argument) : staticEnv
