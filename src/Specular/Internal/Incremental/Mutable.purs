module Specular.Internal.Incremental.Mutable where

import Prelude

import Data.Function.Uncurried (Fn2)
import Effect.Uncurried (EffectFn2, EffectFn3)

data Mutability
foreign import data Mutable :: Mutability
foreign import data Immutable :: Mutability

newtype Field s (m :: Mutability) a = Field String

foreign import data Any :: Type
