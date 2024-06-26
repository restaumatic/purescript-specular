module Specular.Internal.Incremental.MutableArray where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2)

foreign import data MutableArray :: Type -> Type

foreign import empty :: forall a. Effect (MutableArray a)

foreign import push :: forall a. EffectFn2 (MutableArray a) a Unit
foreign import length :: forall a. EffectFn1 (MutableArray a) Int

-- Remove first copy of an element by reference equality.
-- Warning: O(n).
foreign import remove :: forall a. EffectFn2 (MutableArray a) a Unit

-- Call the specified function on each element of the array.
-- The called function is not allowed to change the array.
foreign import iterate :: forall a. EffectFn2 (MutableArray a) (EffectFn1 a Unit) Unit
