module Specular.Internal.Profiling where

import Prelude

import Effect.Uncurried

foreign import enabled :: Boolean

foreign import data Mark :: Type

foreign import none :: Mark

foreign import begin :: EffectFn1 String Mark
foreign import end :: EffectFn1 Mark Unit
