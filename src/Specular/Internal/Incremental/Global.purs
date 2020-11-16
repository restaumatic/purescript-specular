module Specular.Internal.Incremental.Global where

import Prelude

import Effect.Ref (Ref)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried (EffectFn1, EffectFn4, runEffectFn1, runEffectFn4)
import Specular.Internal.Incremental.Ref as Ref

-- * Globals

type StabilizationNum = Int

-- | A sentinel value for `StabilizationNum` meaning that stabilization is not in progress.
stabilizationIsNotInProgress :: StabilizationNum
stabilizationIsNotInProgress = -1


-- | Number of the last started stabilization.
-- | Used as logical timestamp in `Node.changedAt`.
-- |
-- | The timestamp starts at 0.
globalLastStabilizationNum :: Ref StabilizationNum
globalLastStabilizationNum = unsafePerformEffect $ runEffectFn1 Ref.new 0


-- | `globalCurrentStabilizationNum` has the value:
-- | - if stabilization is in progress, then same as `globalLastStabilizationNum`
-- | - otherwise `-1` (which is different than any stabilization number).
-- |
-- | This is used in `isChangingInCurrentStabilization`.
globalCurrentStabilizationNum :: Ref StabilizationNum
globalCurrentStabilizationNum = unsafePerformEffect $ runEffectFn1 Ref.new stabilizationIsNotInProgress


-- | Total refcount of all nodes, for leak checking.
globalTotalRefcount :: Ref Int
globalTotalRefcount = unsafePerformEffect $ runEffectFn1 Ref.new 0
