module Specular.Internal.Effect
  ( module Effect

  , Ref
  , newRef
  , readRef
  , writeRef
  , modifyRef

  , DelayedEffects
  , emptyDelayed
  , pushDelayed
  , unsafeFreezeDelayed
  , sequenceEffects
  ) where

import Prelude

import Effect (Effect)

-- effects

foreign import data Ref :: Type -> Type

foreign import newRef :: forall a. a -> Effect (Ref a)

foreign import readRef :: forall a. Ref a -> Effect a

foreign import writeRef :: forall a. Ref a -> a -> Effect Unit

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
