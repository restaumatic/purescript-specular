module Specular.Callback
  ( Callback
  , mkCallback
  , newCallback

  , triggerCallback
  , attachEvent
  , attachDomEvent
  , attachDyn

  , contramapCallbackDyn
  , contramapCallbackDyn_
  , contramapCallbackDynMaybe
  , contramapCallbackEffect
  , nullCallback
  , dynCallback
  , mbCallback
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Functor.Contravariant (class Contravariant)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Specular.Dom.Browser (Node)
import Specular.Dom.Browser as SpecularDom
import Specular.Dom.Builder.Class (domEventWithSample)
import Specular.Dom.Node.Class (EventType)
import Specular.FRP (class MonadFRP, Dynamic, Event, current, newEvent, pull, readBehavior, readDynamic, subscribeDyn_, subscribeEvent_)

-- | A handle that lets the owner trigger some action with payload of type `a`.
-- | Can be thought of as "inverse of `Event`".
-- |
-- | The `Contravariant` instance can be used to map over the payload.
newtype Callback a = Callback (a -> Effect Unit)

instance contravariantCallback :: Contravariant Callback where
  cmap f (Callback cb) = Callback (cb <<< f)

instance semigroupCallback :: Semigroup (Callback a) where
  append (Callback a) (Callback b) = Callback (\x -> a x *> b x)

instance monoidCallback :: Monoid (Callback a) where
  mempty = nullCallback

-- | Map over the callback payload using a `Dynamic` function.
-- | The Dynamic value will be read at callback trigger time.
contramapCallbackDyn :: forall a b. Dynamic (b -> a) -> Callback a -> Callback b
contramapCallbackDyn fD (Callback cb) = Callback \x -> do
  f <- readDynamic fD
  cb (f x)

contramapCallbackDyn_ :: forall a b. Dynamic a -> Callback a -> Callback b
contramapCallbackDyn_ fD (Callback cb) = Callback \_ -> do
  f <- readDynamic fD
  cb f

-- | Map over the callback payload using a `Dynamic` function. If it returns Nothing, the callback will not be triggered.
-- | The Dynamic value will be read at callback trigger time.
contramapCallbackDynMaybe :: forall a b. Dynamic (b -> Maybe a) -> Callback a -> Callback b
contramapCallbackDynMaybe fD (Callback cb) = Callback \x -> do
  f <- readDynamic fD
  traverse_ cb (f x)

-- | Map over the callback payload using an effectful function.
-- | The effect will be executed at callback trigger time.
contramapCallbackEffect :: forall a b. (b -> Effect a) -> Callback a -> Callback b
contramapCallbackEffect f (Callback cb) = Callback (f >=> cb)

mkCallback :: forall a. (a -> Effect Unit) -> Callback a
mkCallback = Callback

-- | Create a new (`Event`, `Callback`) pair.
-- | Firing the callback will cause the event to occur.
newCallback :: forall m a. MonadEffect m => m { event :: Event a, callback :: Callback a }
newCallback = map (\{event,fire} -> {event, callback: Callback fire}) newEvent

-- | Trigger the action in Effect.
triggerCallback :: forall a. Callback a -> a -> Effect Unit
triggerCallback (Callback cb) = cb

-- | Triger the action each time a given `Event` fires.
attachEvent :: forall m a. MonadFRP m => Event a -> Callback a -> m Unit
attachEvent event (Callback cb) = subscribeEvent_ cb event

attachDyn :: forall m a. MonadFRP m => Dynamic a -> Callback a -> m Unit
attachDyn dyn (Callback cb) = subscribeDyn_ cb dyn

-- | Trigger a callback each time a DOM event fires.
-- | The payload will be the DOM event. It is advised to `cmap` it to a nicer type.
attachDomEvent :: forall m. MonadFRP m => EventType -> Node -> Callback SpecularDom.Event -> m Unit
attachDomEvent eventType node callback = do
  event <- domEventWithSample pure eventType node
  attachEvent event callback

-- | A Callback that just ignores the input values.
nullCallback :: forall a. Callback a
nullCallback = Callback (\_ -> pure unit)

dynCallback :: forall a. Dynamic (Callback a) -> Callback a
dynCallback dynCb = Callback \x -> do
  cb <- readDynamic dynCb
  triggerCallback cb x

mbCallback :: forall a. Callback a -> Callback (Maybe a)
mbCallback (Callback cb) = Callback $ traverse_ cb
