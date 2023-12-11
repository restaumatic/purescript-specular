module Specular.Internal.Effect
  ( DelayedEffects
  , emptyDelayed
  , pushDelayed
  , unsafeFreezeDelayed
  , sequenceEffects
  , nextMicrotask
  ) where

import Prelude

import Effect (Effect)

foreign import data DelayedEffects :: Type

foreign import emptyDelayed :: Effect DelayedEffects

foreign import pushDelayed :: DelayedEffects -> Effect Unit -> Effect Unit

foreign import unsafeFreezeDelayed :: DelayedEffects -> Effect (Array (Effect Unit))

foreign import sequenceEffects :: Array (Effect Unit) -> Effect Unit

foreign import nextMicrotask :: Effect Unit -> Effect Unit
