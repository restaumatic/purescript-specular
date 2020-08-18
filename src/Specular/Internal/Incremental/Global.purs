module Specular.Internal.Incremental.Global where

import Prelude

import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)

-- * Globals

globalCurrentStabilizationNum :: Ref Int
globalCurrentStabilizationNum = unsafePerformEffect $ Ref.new (-1)
