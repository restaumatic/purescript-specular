module Specular.FRP.WeakDynamic (
    WeakDynamic 
  , unWeakDynamic
  , changedW
  , weaken
  , holdWeakDyn
  , switchWeakDyn
  , subscribeWeakDyn
  , subscribeWeakDyn_
  , attachWeakDynWith
  , tagWeakDyn
  , uniqWeakDynBy
) where

import Prelude

import Control.Monad.IOSync (IOSync)
import Data.Foldable (traverse_)
import Data.Functor.Compose (Compose(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Specular.FRP.Base (class MonadFRP, Dynamic, Event, attachDynWith, changed, filterMapEvent, holdDyn, never, newEvent, subscribeDyn_, switch, uniqDynBy)

-- | A primitive similar to Dynamic. The difference is: while Dynamic always
-- | has a value, WeakDynamic has a value always after some point, but for
-- | some time after construction it may be without value.
-- |
-- | A Dynamic can be always converted to a WeakDynamic using `weaken`.
--
-- Invariant: Once the Dynamic changed to Just, it will never again turn to Nothing.
newtype WeakDynamic a = WeakDynamic (Compose Dynamic Maybe a)

unWeakDynamic :: forall a. WeakDynamic a -> Dynamic (Maybe a)
unWeakDynamic (WeakDynamic (Compose mdyn)) = mdyn

mkWeakDynamic :: forall a. Dynamic (Maybe a) -> WeakDynamic a
mkWeakDynamic = WeakDynamic <<< Compose

derive newtype instance functorWeakDynamic :: Functor WeakDynamic
derive newtype instance applyWeakDynamic :: Apply WeakDynamic
derive newtype instance applicativeWeakDynamic :: Applicative WeakDynamic

instance bindWeakDynamic :: Bind WeakDynamic where
  bind (WeakDynamic (Compose mdyn)) k = WeakDynamic $ Compose $ do
     value <- mdyn
     map join $ unWeakDynamic $ traverse k value

-- | Convert a Dynamic to a WeakDynamic. It will have the same value as the
-- | original Dynamic, and will change whenever the original Dynamic changes.
weaken :: forall a. Dynamic a -> WeakDynamic a
weaken = WeakDynamic <<< Compose <<< map Just

-- | An Event that fires every time a WeakDynamic changes, with the new value.
changedW :: forall a. WeakDynamic a -> Event a
changedW = filterMapEvent id <<< changed <<< unWeakDynamic

-- | Make a WeakDynamic that will have no value, but will acquire one when the Event fires.
-- | It will also change every time the Event fires.
holdWeakDyn :: forall m a. MonadFRP m => Event a -> m (WeakDynamic a)
holdWeakDyn change = WeakDynamic <<< Compose <$> holdDyn Nothing (Just <$> change)

-- | Make an Event that occurs when the given WeakDynamic has a value, and the
-- | value (an Event) occurs.
switchWeakDyn :: forall a. WeakDynamic (Event a) -> Event a
switchWeakDyn (WeakDynamic (Compose mdyn)) = switch $ map (fromMaybe never) mdyn

-- | Invoke the handler immediately if the WeakDynamic has a value currently,
-- | and invoke it every time it changes, until cleanup.
subscribeWeakDyn_ ::
     forall m a
   . MonadFRP m
  => (a -> IOSync Unit)
  -> WeakDynamic a
  -> m Unit
subscribeWeakDyn_ handler (WeakDynamic (Compose mdyn)) =
  subscribeDyn_ (traverse_ handler) mdyn

-- | Invoke the handler immediately if the WeakDynamic has a value currently,
-- | and invoke it every time it changes, until cleanup.
subscribeWeakDyn ::
     forall m a b
   . MonadFRP m
  => (a -> IOSync b)
  -> WeakDynamic a
  -> m (WeakDynamic b)
subscribeWeakDyn handler wdyn = do
  {event,fire} <- newEvent
  result <- holdWeakDyn event
  subscribeWeakDyn_ (handler >=> fire) wdyn
  pure result

attachWeakDynWith :: forall a b c. (a -> b -> c) -> WeakDynamic a -> Event b -> Event c
attachWeakDynWith f wdyn event =
  filterMapEvent id $ attachDynWith (\a b -> f <$> a <*> pure b) (unWeakDynamic wdyn) event

tagWeakDyn :: forall a. WeakDynamic a -> Event Unit -> Event a
tagWeakDyn wdyn event =
  filterMapEvent id $ attachDynWith (\a b -> a) (unWeakDynamic wdyn) event

-- | Make a WeakDynamic that only changes value when the input WeakDynamic changes
-- | value, and the new value is not equal to the previous value with respect to
-- | the given equality test.
uniqWeakDynBy :: forall m a. MonadFRP m => (a -> a -> Boolean) -> WeakDynamic a -> m (WeakDynamic a)
uniqWeakDynBy eq wdyn = mkWeakDynamic <$> uniqDynBy (liftEqMaybe eq) (unWeakDynamic wdyn)

liftEqMaybe :: forall a. (a -> a -> Boolean) -> Maybe a -> Maybe a -> Boolean
liftEqMaybe eq =
  case _, _ of
    Just x,  Just y  -> eq x y
    Nothing, Nothing -> true
    _      , _       -> false

