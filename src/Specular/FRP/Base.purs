module Specular.FRP.Base (
    Event
  , never
  , leftmost
--  , mergeEvents

  , filterEvent
  , filterMapEvent

  , Pull
  , pull

  , Behavior
  , sampleAt
  , readBehavior

  , Dynamic
  , current
  , changed
  , changed_
  , switch
  , tagDyn
  , attachDynWith
  , latestJust
  , readDynamic
  , newDynamic

  , holdDyn
  , foldDyn
  , foldDynMaybe
  , holdUniqDynBy
  , uniqDynBy

  , newEvent
  , newBehavior

  , subscribeEvent_
  , subscribeDyn_
  , subscribeDyn

  , _subscribeEvent

  , class MonadFRP

  , traceEventIO
  , traceDynIO
) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Cleanup (class MonadCleanup, onCleanup)
import Control.Monad.Reader (ask)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (cons, unsnoc)
import Data.Array as Array
import Data.Foldable (for_)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn2, mkEffectFn2, runEffectFn1, runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Incremental.Internal as I
import Incremental.Internal.Node as I
import Partial.Unsafe (unsafeCrashWith)
import Specular.Internal.Effect (DelayedEffects, Ref, emptyDelayed, modifyRef, newRef, pushDelayed, readRef, sequenceEffects, unsafeFreezeDelayed, writeRef)
import Specular.Internal.RIO (RIO, rio, runRIO)
import Specular.Internal.RIO as RIO
import Specular.Internal.UniqueMap.Mutable as UMM
import Unsafe.Coerce (unsafeCoerce)

-------------------------------------------------------------

-- | Behaviors are time-changing values that can be read, but not subscribed to.
--
-- A Behavior should never change during a frame.
--
-- Can be composed using Monad instance.
newtype Behavior a = Behavior (Dynamic a)
-- Behavior is represented by a computation that reads its value.

-- | Read a value of a Behavior.
readBehavior :: forall a. Behavior a -> Effect a
readBehavior (Behavior read) = unsafeCrashWith "readBehavior"

type Pull = Effect

pull = identity

derive newtype instance functorBehavior :: Functor Behavior
derive newtype instance applyBehavior :: Apply Behavior
derive newtype instance applicativeBehavior :: Applicative Behavior
derive newtype instance bindBehavior :: Bind Behavior
instance monadBehavior :: Monad Behavior

-------------------------------------------------------------

type Unsubscribe = Effect Unit

-- | A source of occurences.
-- |
-- | During a frame, an Event occurs at most once with a value of type a.
-- | 
-- | Event is a functor. It is not, however, an Applicative. There is no
-- | meaningful interpretation of `pure` (when would the event occur?).
-- | There is an interpretation of `apply` (Event that fires when the input
-- | events coincide), but it's not very useful.
newtype Event a = Event (I.Node a)

-- We represent an Event with:
--  - a Behavior that tells whether this Event occurs during a given frame,
--    and if so, its occurence value,
--  - subscription function.

instance functorEvent :: Functor Event where
  map f (Event e) = unsafeCrashWith "Event.map"

-- | An Event that never occurs.
never :: forall a. Event a
never = unsafeCoerce 1

filterMapEventB :: forall a b. (a -> Behavior (Maybe b)) -> Event a -> Event b
filterMapEventB f (Event _) = unsafeCrashWith "filterMapEventB"

mapEventB :: forall a b. (a -> Behavior b) -> Event a -> Event b
mapEventB f (Event _) =
  unsafeCrashWith "mapEventB"

sampleAt :: forall a b. Event (a -> b) -> Behavior a -> Event b
sampleAt event behavior = mapEventB (\f -> f <$> behavior) event

filterMapEvent :: forall a b. (a -> Maybe b) -> Event a -> Event b
filterMapEvent f = filterMapEventB (pure <<< f)

filterEvent :: forall a. (a -> Boolean) -> Event a -> Event a
filterEvent f = filterMapEvent (\x -> if f x then Just x else Nothing)

subscribeEvent_ :: forall m a. MonadEffect m => MonadCleanup m => (a -> Effect Unit) -> Event a -> m Unit
subscribeEvent_ handler event = do
  unsub <- liftEffect $ runEffectFn2 _subscribeEvent handler event
  onCleanup unsub

_subscribeEvent :: forall a. EffectFn2 (a -> Effect Unit) (Event a) Unsubscribe
_subscribeEvent = mkEffectFn2 \handler (Event node) -> do
  arrayRef <- newRef []
  let handler value = do
        modifyRef arrayRef (cons value)
        tailRecM (\_ -> do
          elem <- popRef arrayRef
          case elem of
            Just value -> do
              handler value
              pure $ Loop unit
            Nothing -> pure (Done unit)
          ) unit
  I.addObserver node handler

  where
    popRef :: forall s. Ref (Array s) -> Effect (Maybe s)
    popRef ref = do
      array <- readRef ref
      case unsnoc array of
        Nothing -> pure Nothing
        Just { init, last } -> do
          writeRef ref init
          pure (Just last)

-- | Create an Event that can be triggered externally.
-- | Each `fire` will run a frame where the event occurs.
newEvent :: forall m a. MonadEffect m => m { event :: Event a, fire :: a -> Effect Unit }
newEvent = liftEffect do
  effectCrash "newEvent"

-- | Create a new Behavior whose value can be modified outside a frame.
newBehavior :: forall m a. MonadEffect m => a -> m { behavior :: Behavior a, set :: a -> Effect Unit }
newBehavior initialValue = liftEffect $ newBehaviorEffect initialValue

newBehaviorEffect :: forall a. a -> Effect { behavior :: Behavior a, set :: a -> Effect Unit }
newBehaviorEffect initialValue = do
  { dynamic: Dynamic node, set } <- newDynamic initialValue
  pure
    { behavior: Behavior node
    , set
    }

-- | An Event that occurs when any of the events occur.
-- | If some of them occur simultaneously, the occurence value is that of the
-- | leftmost one.
leftmost :: forall a. Array (Event a) -> Event a
leftmost events =
  unsafeCrashWith "leftmost"

findFirstM :: forall m a b. Monad m => (a -> m (Maybe b)) -> Array a -> m (Maybe b)
findFirstM f array =
  case Array.uncons array of

    Just {head,tail} -> do
      m_value <- f head
      case m_value of
        Just x -> pure (Just x)
        Nothing -> findFirstM f tail

    Nothing ->
      pure Nothing

-----------------------------------------------------------------

-- | `Dynamic a` represents a _dynamically changing value_ of type `a`. The
-- | current value may be queried at any time (using `current`), and it's
-- | possible to be notified of changes (using `changed`).
newtype Dynamic a = Dynamic (I.Node a)

-- | The Behavior representing the current value of the Dynamic.
-- | When it is changing (the change event occurs), it has the new value.
-- |
-- | The value of `current x` is always the value of the latest occurence of
-- | `changed x`, if it has ever occured.
current :: forall a. Dynamic a -> Behavior a
current (Dynamic _) = unsafeCrashWith "Dynamic.current"

-- | An Event that fires with the new value every time the Dynamic changes.
changed :: forall a. Dynamic a -> Event a
changed (Dynamic _) = unsafeCrashWith "changed"

-- | An Event that fires every time the Dynamic changes.
changed_ :: forall a. Dynamic a -> Event Unit
changed_ (Dynamic _) = unsafeCrashWith "changed_"

instance functorDynamic :: Functor Dynamic where
  map f (Dynamic node) = Dynamic $ unsafePerformEffect do
    runEffectFn2 I.map f node

instance applyDynamic :: Apply Dynamic where
  apply (Dynamic f) (Dynamic x) =
    unsafeCrashWith "applyDynamic"

instance applicativeDynamic :: Applicative Dynamic where
  pure x = Dynamic $ unsafePerformEffect do
    runEffectFn1 I.constant x


-- | `foldDyn f x e` - Make a Dynamic that will have the initial value `x`,
-- | and every time `e` fires, its value will update by applying `f` to the
-- | event occurence value and the old value.
-- |
-- | On cleanup, the Dynamic will stop updating in response to the event.
foldDyn :: forall m a b. MonadFRP m => (a -> b -> b) -> b -> Event a -> m (Dynamic b)
foldDyn f initial (Event event) = do
  effectCrash "foldDyn"

effectCrash msg = unsafeCoerce ((\_ -> unsafeCrashWith msg) :: forall a. Unit -> a)

-- | Construct a new root Dynamic that can be changed from `Effect`-land.
newDynamic :: forall m a. MonadEffect m => a -> m { dynamic :: Dynamic a, read :: Effect a, set :: a -> Effect Unit }
newDynamic initial = liftEffect do
  var <- runEffectFn1 I.newVar initial
  pure
    { dynamic: Dynamic (I.readVar var)
    , read: effectCrash "Dynamic.read"
    , set: \x -> do
        runEffectFn2 I.setVar var x
        I.stabilize
    }

-- | Like `foldDyn`, but the Dynamic will not update if the folding function
-- | returns Nothing.
foldDynMaybe :: forall m a b. MonadFRP m => (a -> b -> Maybe b) -> b -> Event a -> m (Dynamic b)
foldDynMaybe f initial (Event event) = do
  effectCrash "foldDyn"

-- | `holdDyn initialValue event` returns a `Dynamic` that starts with `initialValue`, and changes to the occurence value of `event` when `event` fires
holdDyn :: forall m a. MonadFRP m => a -> Event a -> m (Dynamic a)
holdDyn = foldDyn (\x _ -> x)

holdUniqDynBy :: forall m a. MonadFRP m => (a -> a -> Boolean) -> a -> Event a -> m (Dynamic a)
holdUniqDynBy eq = foldDynMaybe (\new old -> if eq new old then Nothing else Just new)

-- | Make a Dynamic that only changes value when the input Dynamic changes
-- | value, and the new value is not equal to the previous value with respect to
-- | the given equality test.
uniqDynBy :: forall m a. MonadFRP m => (a -> a -> Boolean) -> Dynamic a -> m (Dynamic a)
uniqDynBy eq dyn = do
  initialValue <- pull $ readBehavior $ current dyn
  holdUniqDynBy eq initialValue (changed dyn)

-- | Make an Event that occurs when the current value of the given Dynamic (an Event) occurs.
switch :: forall a. Dynamic (Event a) -> Event a
switch (Dynamic _) =
  unsafeCrashWith "switch"

instance bindDynamic :: Bind Dynamic where
  bind d f = unsafeCrashWith "bind"

instance monadDynamic :: Monad Dynamic

subscribeDyn_
  :: forall m a
   . MonadFRP m
  => (a -> Effect Unit)
  -> Dynamic a
  -> m Unit
subscribeDyn_ handler (Dynamic {value, change}) = do
  currentValue <- pull $ readBehavior value
  liftEffect $ handler currentValue
  subscribeEvent_ handler (mapEventB (\_ -> value) change)

subscribeDyn ::
     forall m a b
   . MonadFRP m
  => (a -> Effect b)
  -> Dynamic a
  -> m (Dynamic b)
subscribeDyn handler dyn = do
  {event,fire} <- newEvent
  currentValue <- pull $ readBehavior $ current dyn
  initialResult <- liftEffect $ handler currentValue
  subscribeEvent_ (handler >=> fire) $ changed dyn
  holdDyn initialResult event

tagDyn :: forall a. Dynamic a -> Event Unit -> Event a
tagDyn dyn event = sampleAt (identity <$ event) (current dyn)

attachDynWith :: forall a b c. (a -> b -> c) -> Dynamic a -> Event b -> Event c
attachDynWith f dyn event = sampleAt (flip f <$> event) (current dyn)

-- | Returns a Dynamic that holds the latest `Just` value of the input Dynamic
-- | after execution of this function. If the input currently has value `Nothing`, the resulting
-- | Dynamic will have the value `Nothing` until the input changes to a `Just`.
-- |
-- | The resulting Dynamic changes when the input changes to a value that is a `Just`,
-- | and doesn't change when the input changes to `Nothing`.
latestJust :: forall m a. MonadFRP m => Dynamic (Maybe a) -> m (Dynamic (Maybe a))
latestJust dyn = do
  currentValue <- pull $ readBehavior $ current dyn
  foldDynMaybe (\new _ -> map Just new) currentValue (changed dyn)

readDynamic :: forall m a. MonadEffect m => Dynamic a -> m a
readDynamic = pull <<< readBehavior <<< current

-- | A "type class alias" for the constraints required by most FRP primitives.
class (MonadEffect m, MonadCleanup m) <= MonadFRP m
instance monadFRP :: (MonadEffect m, MonadCleanup m) => MonadFRP m


traceEventIO :: forall a. (a -> Effect Unit) -> Event a -> Event a
traceEventIO handler e = e

traceDynIO :: forall a. (a -> Effect Unit) -> Dynamic a -> Dynamic a
traceDynIO handler d = d

--- Lifted instances

-- | This instance allows use of the Boolean operators `(||)` and `(&&)` directly on `Dynamic Bool`.
instance heytingAlgebraDynamic :: HeytingAlgebra a => HeytingAlgebra (Dynamic a) where
  tt = pure tt
  ff = pure ff
  implies = lift2 implies
  conj = lift2 conj
  disj = lift2 disj
  not = map not

instance semigroupDynamic :: Semigroup a => Semigroup (Dynamic a) where
  append = lift2 append

instance monoidDynamic :: Monoid a => Monoid (Dynamic a) where
  mempty = pure mempty
