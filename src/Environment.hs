module Environment
  ( Environment
  , lookup
  , extend
  )
where

import Prelude hiding (lookup)
import qualified Prelude (lookup)

type Name = String

type Environment a = [(Name, a)]

extend :: Name -> a -> Environment a -> Environment a
extend name value env = (name, value) : env

lookup :: Name -> Environment a -> Maybe a
lookup = Prelude.lookup