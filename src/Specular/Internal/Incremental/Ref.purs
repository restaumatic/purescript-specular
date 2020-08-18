module Specular.Internal.Incremental.Ref where

import Prelude

import Effect.Ref (Ref)
import Effect.Uncurried (EffectFn1, EffectFn2)

foreign import new :: forall a. EffectFn1 a (Ref a)
foreign import read :: forall a. EffectFn1 (Ref a) a
foreign import write :: forall a. EffectFn2 (Ref a) a Unit
