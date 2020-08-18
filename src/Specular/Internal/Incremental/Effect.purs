module Specular.Internal.Incremental.Effect where

import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn2, runEffectFn3)
import Specular.Internal.Incremental.Optional (Optional)
import Specular.Internal.Incremental.Optional (none) as Optional

foreign import _foreachUntil :: forall a b. EffectFn3 (Optional a) (Array a) (EffectFn1 a (Optional b)) (Optional b)

foreachUntil :: forall a b. EffectFn2 (Array a) (EffectFn1 a (Optional b)) (Optional b)
foreachUntil = mkEffectFn2 \array fn ->
  runEffectFn3 _foreachUntil Optional.none array fn
