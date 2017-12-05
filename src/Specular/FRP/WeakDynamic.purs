module Specular.FRP.WeakDynamic (
    WeakDynamic 
  , changedW
  , weaken
  , holdWeakDyn
  , subscribeWeakDyn_
) where

import Prelude

import Data.Foldable (traverse_)
import Data.Functor.Compose (Compose(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Specular.FRP.Base (class MonadHold, class MonadHost, Dynamic, Event, changed, filterMapEvent, holdDyn, subscribeDyn_)

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
holdWeakDyn :: forall m a. MonadHold m => Event a -> m (WeakDynamic a)
holdWeakDyn change = WeakDynamic <<< Compose <$> holdDyn Nothing (Just <$> change)

-- | Invoke the handler immediately if the WeakDynamic has a value currently,
-- | and invoke it every time it changes, until cleanup.
subscribeWeakDyn_ ::
     forall io m a
   . MonadHost io m
  => Applicative io
  => (a -> io Unit)
  -> WeakDynamic a
  -> m Unit
subscribeWeakDyn_ handler (WeakDynamic (Compose mdyn)) =
  subscribeDyn_ (traverse_ handler) mdyn
