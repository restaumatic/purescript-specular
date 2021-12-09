module Specular.FRP.WeakDynamic
  ( WeakDynamic
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

import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Specular.FRP.Base (class MonadFRP, Dynamic, Event, attachDynWith, changed, filterMapEvent, holdDyn, never, newEvent, subscribeDyn_, switch, uniqDynBy)

-- | A primitive similar to Dynamic. The difference is: while Dynamic always
-- | has a value, WeakDynamic has a value always after some point, but for
-- | some time after construction it may be without value.
-- |
-- | A Dynamic can be always converted to a WeakDynamic using `weaken`.
--
-- Invariant: Once the Dynamic changed to Just, it will never again turn to Nothing.
newtype WeakDynamic a = WeakDynamic (MaybeT Dynamic a)

unWeakDynamic :: forall a. WeakDynamic a -> Dynamic (Maybe a)
unWeakDynamic (WeakDynamic (MaybeT mdyn)) = mdyn

mkWeakDynamic :: forall a. Dynamic (Maybe a) -> WeakDynamic a
mkWeakDynamic = WeakDynamic <<< MaybeT

derive newtype instance functorWeakDynamic :: Functor WeakDynamic
derive newtype instance applyWeakDynamic :: Apply WeakDynamic
derive newtype instance applicativeWeakDynamic :: Applicative WeakDynamic
derive newtype instance bindWeakDynamic :: Bind WeakDynamic
derive newtype instance monadWeakDynamic :: Monad WeakDynamic

-- | Convert a Dynamic to a WeakDynamic. It will have the same value as the
-- | original Dynamic, and will change whenever the original Dynamic changes.
weaken :: forall a. Dynamic a -> WeakDynamic a
weaken = WeakDynamic <<< MaybeT <<< map Just

-- | An Event that fires every time a WeakDynamic changes, with the new value.
changedW :: forall a. WeakDynamic a -> Event a
changedW = filterMapEvent identity <<< changed <<< unWeakDynamic

-- | Make a WeakDynamic that will have no value, but will acquire one when the Event fires.
-- | It will also change every time the Event fires.
holdWeakDyn :: forall m a. MonadFRP m => Event a -> m (WeakDynamic a)
holdWeakDyn change = WeakDynamic <<< MaybeT <$> holdDyn Nothing (Just <$> change)

-- | Make an Event that occurs when the given WeakDynamic has a value, and the
-- | value (an Event) occurs.
switchWeakDyn :: forall a. WeakDynamic (Event a) -> Event a
switchWeakDyn (WeakDynamic (MaybeT mdyn)) = switch $ map (fromMaybe never) mdyn

-- | Invoke the handler immediately if the WeakDynamic has a value currently,
-- | and invoke it every time it changes, until cleanup.
subscribeWeakDyn_
  :: forall m a
   . MonadFRP m
  => (a -> Effect Unit)
  -> WeakDynamic a
  -> m Unit
subscribeWeakDyn_ handler (WeakDynamic (MaybeT mdyn)) =
  subscribeDyn_ (traverse_ handler) mdyn

-- | Invoke the handler immediately if the WeakDynamic has a value currently,
-- | and invoke it every time it changes, until cleanup.
subscribeWeakDyn
  :: forall m a b
   . MonadFRP m
  => (a -> Effect b)
  -> WeakDynamic a
  -> m (WeakDynamic b)
subscribeWeakDyn handler wdyn = do
  { event, fire } <- newEvent
  result <- holdWeakDyn event
  subscribeWeakDyn_ (handler >=> fire) wdyn
  pure result

attachWeakDynWith :: forall a b c. (a -> b -> c) -> WeakDynamic a -> Event b -> Event c
attachWeakDynWith f wdyn event =
  filterMapEvent identity $ attachDynWith (\a b -> f <$> a <*> pure b) (unWeakDynamic wdyn) event

tagWeakDyn :: forall a. WeakDynamic a -> Event Unit -> Event a
tagWeakDyn wdyn event =
  filterMapEvent identity $ attachDynWith (\a _b -> a) (unWeakDynamic wdyn) event

-- | Make a WeakDynamic that only changes value when the input WeakDynamic changes
-- | value, and the new value is not equal to the previous value with respect to
-- | the given equality test.
uniqWeakDynBy :: forall m a. MonadFRP m => (a -> a -> Boolean) -> WeakDynamic a -> m (WeakDynamic a)
uniqWeakDynBy eq wdyn = mkWeakDynamic <$> uniqDynBy (liftEqMaybe eq) (unWeakDynamic wdyn)

-- | Lift an equivalence relation on `a` to an equivalence relation on `Maybe a`.
liftEqMaybe :: forall a. (a -> a -> Boolean) -> Maybe a -> Maybe a -> Boolean
liftEqMaybe eq =
  case _, _ of
    Just x, Just y -> eq x y
    Nothing, Nothing -> true
    _, _ -> false

