module Specular.Internal.Effect
  ( module Effect

  , Ref
  , newRef
  , readRef
  , writeRef
  , _newRef
  , _readRef
  , _writeRef
  , modifyRef

  , DelayedEffects
  , emptyDelayed
  , pushDelayed
  , unsafeFreezeDelayed
  , sequenceEffects
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)

-- effects

foreign import data Ref :: Type -> Type

newRef :: forall a. a -> Effect (Ref a)
newRef x = runEffectFn1 _newRef x

foreign import _newRef :: forall a. EffectFn1 a (Ref a)

readRef :: forall a. Ref a -> Effect a
readRef x = runEffectFn1 _readRef x

foreign import _readRef :: forall a. EffectFn1 (Ref a) a

writeRef :: forall a. Ref a -> a -> Effect Unit
writeRef x y = runEffectFn2 _writeRef x y

foreign import _writeRef :: forall a. EffectFn2 (Ref a) a Unit

modifyRef :: forall a. Ref a -> (a -> a) -> Effect Unit
modifyRef ref f = do
  x <- readRef ref
  writeRef ref (f x)

-- delayed

foreign import data DelayedEffects :: Type

foreign import emptyDelayed :: Effect DelayedEffects

foreign import pushDelayed :: DelayedEffects -> Effect Unit -> Effect Unit

foreign import unsafeFreezeDelayed :: DelayedEffects -> Effect (Array (Effect Unit))

foreign import sequenceEffects :: Array (Effect Unit) -> Effect Unit
