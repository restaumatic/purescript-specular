module Specular.Internal.Incremental.Array where

import Prelude

import Effect.Uncurried (EffectFn1, EffectFn2)

foreign import iterate :: forall a. EffectFn2 (Array a) (EffectFn1 a Unit) Unit
