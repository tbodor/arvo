module Environment
  ( Environment
  , lookup
  , extend
  , names
  , bindings
  , fromList
  , empty
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

names :: Environment a -> [Name]
names = map fst

bindings :: Environment a -> [(Name, a)]
bindings = id 

fromList :: [(Name, a)] -> Environment a
fromList = id

empty :: Environment a
empty = []