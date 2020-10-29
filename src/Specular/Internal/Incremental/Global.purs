module Specular.Internal.Incremental.Global where

import Prelude

import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)

-- * Globals

globalCurrentStabilizationNum :: Ref Int
globalCurrentStabilizationNum = unsafePerformEffect $ Ref.new (-1)


-- | Total refcount of all nodes, for leak checking.
globalTotalRefcount :: Ref Int
globalTotalRefcount = unsafePerformEffect $ Ref.new 0
