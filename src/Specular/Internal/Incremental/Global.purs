module Specular.Internal.Incremental.Global where

import Prelude

import Effect.Ref (Ref)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried (EffectFn1, EffectFn4, runEffectFn1, runEffectFn4)
import Specular.Internal.Incremental.Ref as Ref

-- * Globals


-- | Number of the last started stabilization.
-- | Used as logical timestamp in `Node.changedAt`.
-- |
-- | The timestamp starts at 0.
globalLastStabilizationNum :: Ref Int
globalLastStabilizationNum = unsafePerformEffect $ runEffectFn1 Ref.new 0

-- | `globalCurrentStabilizationNum` has the value:
-- | - if stabilization is in progress, then same as `globalLastStabilizationNum`
-- | - otherwise `-1` (which is different than any stabilization number).
-- |
-- | This is used in `isChangingInCurrentStabilization`.
globalCurrentStabilizationNum :: Ref Int
globalCurrentStabilizationNum = unsafePerformEffect $ runEffectFn1 Ref.new (-1)


-- | Total refcount of all nodes, for leak checking.
globalTotalRefcount :: Ref Int
globalTotalRefcount = unsafePerformEffect $ runEffectFn1 Ref.new 0
