module Syntax
  ( Name
  , Term (..)
  )
where

type Name = String

data Term
  = Variable Name
  | Lambda Name Term
  | Application Term Term
  deriving (Eq)


instance Show Term where
  show (Variable v) = v
  show (Lambda parameter body) = "λ" ++ parameter ++ "." ++ show body
  show (Application function argument) = showLeft function ++ showRight argument
    where
      showLeft  (Lambda _ _)   = "(" ++ show function ++ ")"
      showLeft  _              = show function
      showRight (Variable x)   = ' ' : x
      showRight _              = " (" ++ show argument ++ ")"


{-
  -- Use this to merge λx.λy.λz. ... to λx y z. ...
  show (Lambda parameter body)  = "λ" ++ parameter ++ showBody body
    where
      showBody (Lambda x y) = " " ++ x ++ showBody y
      showBody expr         = "." ++ show expr
-}
