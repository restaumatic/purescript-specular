module Specular.Internal.Queue where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2)

-- | A mutable FIFO queue.
foreign import data Queue :: Type -> Type

-- | Create a new empty queue.
foreign import new :: forall a. Effect (Queue a)

-- | Add an element onto the end of the queue.
foreign import enqueue :: forall a. EffectFn2 (Queue a) a Unit

-- | Iterate over the queue, calling the specified function on each element in sequence.
-- |
-- | Postcondition: When `drain` returns, the queue is empty.
-- |
-- | If the function calls `enqueue`, the newly added elements will be processed in the run of `drain`.
-- |
-- | If the function calls `drain`, the inner `drain` will consume all elements
-- | and it will be the last invocation of the callback function in the outer
-- | `drain`.
foreign import drain :: forall a. EffectFn2 (Queue a) (EffectFn1 a Unit) Unit
