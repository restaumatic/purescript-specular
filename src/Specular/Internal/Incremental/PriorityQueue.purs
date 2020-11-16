module Specular.Internal.Incremental.PriorityQueue where

import Prelude

import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn4)
import Specular.Internal.Incremental.Mutable (Any, Field, Mutable)
import Specular.Internal.Incremental.Optional (Optional)

foreign import data PQ :: Type -> Type

foreign import new :: forall a.
  EffectFn4
    (Optional Any)                 -- Optional.none
    (Field a Mutable Int)          -- Priority. Must be a non-negative integer.
    (Field a Mutable Boolean)      -- Is the entry present in this queue?
    (Field a Mutable (Optional a)) -- Next entry with the same priority
  (PQ a)

foreign import add :: forall a. EffectFn2 (PQ a) a Boolean
foreign import removeMin :: forall a. EffectFn1 (PQ a) (Optional a)
foreign import drain :: forall a. EffectFn2 (PQ a) (EffectFn1 a Unit) Unit
