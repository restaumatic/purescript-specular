module Specular.FRP.Base (
    Event
  , never
  , leftmost
--  , mergeEvents

  , filterEvent
  , filterMapEvent
  , filterJustEvent

  , module X
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
  , uniqDyn

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
import Data.Array as Array
import Data.Function.Uncurried (mkFn2)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn2, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2, runEffectFn3)
import Effect.Unsafe (unsafePerformEffect)
import Specular.Internal.Incremental (addObserver, bind_, constant, fold, leftmost, map, map2, mapOptional, newEvent, newVar, readEvent, readVar, removeObserver, sample, setVar, stabilize, switch, traceChanges, triggerEvent) as I
import Specular.Internal.Incremental.Node (Node)
import Specular.Internal.Incremental.Node as Node
import Specular.Internal.Incremental.Optional as Optional
import Partial.Unsafe (unsafeCrashWith)
import Specular.FRP.Internal.Frame (Pull) as X
import Specular.Internal.Queue (Queue)
import Specular.Internal.Queue as Queue
import Unsafe.Coerce (unsafeCoerce)

-------------------------------------------------------------

-- | Behaviors are time-changing values that can be read, but not subscribed to.
--
-- A Behavior should never change during a frame.
--
-- Can be composed using Monad instance.
newtype Behavior a = Behavior (Dynamic a)

-- | Read a value of a Behavior.
readBehavior :: forall a. Behavior a -> Effect a
readBehavior (Behavior (Dynamic node)) = do
  readNode node

type Pull = Effect

pull :: forall a m. MonadEffect m => Effect a -> m a
pull = liftEffect

derive newtype instance functorBehavior :: Functor Behavior
derive newtype instance applyBehavior :: Apply Behavior
derive newtype instance applicativeBehavior :: Applicative Behavior
derive newtype instance bindBehavior :: Bind Behavior
instance monadBehavior :: Monad Behavior

readNode :: forall a. Node a -> Effect a
readNode node = do
  -- HACK: For now we have to observe node to be sure we have the latest value
  let handler = mkEffectFn1 \_ -> pure unit
  runEffectFn2 I.addObserver node handler
  value <- runEffectFn1 Node.get_value node
  runEffectFn2 I.removeObserver node handler
  pure (Optional.fromSome value)

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
newtype Event a = Event (Node a)

-- We represent an Event with:
--  - a Behavior that tells whether this Event occurs during a given frame,
--    and if so, its occurence value,
--  - subscription function.

instance functorEvent :: Functor Event where
  map f (Event node) = Event $ unsafePerformEffect do
    n <- runEffectFn2 I.mapOptional (Optional.some <<< f) node
    runEffectFn2 Node.annotate n "mapEvent"
    pure n

-- | An Event that never occurs.
never :: forall a. Event a
never = Event (I.readEvent (unsafePerformEffect I.newEvent))

sampleAt :: forall a b. Event (a -> b) -> Behavior a -> Event b
sampleAt (Event clock) (Behavior (Dynamic signal)) = Event $ unsafePerformEffect do
  n <- runEffectFn3 I.sample (mkFn2 \a b -> Optional.some (b a)) signal clock
  runEffectFn2 Node.annotate n "sampleAt"
  pure n

filterMapEvent :: forall a b. (a -> Maybe b) -> Event a -> Event b
filterMapEvent f (Event node) = Event $ unsafePerformEffect do
  n <- runEffectFn2 I.mapOptional (maybe Optional.none Optional.some <<< f) node
  runEffectFn2 Node.annotate n "filterMapEvent"
  pure n

filterEvent :: forall a. (a -> Boolean) -> Event a -> Event a
filterEvent f (Event node) = Event $ unsafePerformEffect do
  n <- runEffectFn2 I.mapOptional (\x -> if f x then Optional.some x else Optional.none) node
  runEffectFn2 Node.annotate n "filterEvent"
  pure n

subscribeNode :: forall m a. MonadEffect m => MonadCleanup m => (a -> Effect Unit) -> Node a -> m Unit
subscribeNode handler event = do
  unsub <- liftEffect $ runEffectFn2 _subscribeNode handler event
  onCleanup unsub

filterJustEvent :: forall a. Event (Maybe a) -> Event a
filterJustEvent = filterMapEvent identity

subscribeEvent_ :: forall m a. MonadEffect m => MonadCleanup m => (a -> Effect Unit) -> Event a -> m Unit
subscribeEvent_ handler (Event node) = subscribeNode handler node

_subscribeEvent :: forall a. EffectFn2 (a -> Effect Unit) (Event a) Unsubscribe
_subscribeEvent = mkEffectFn2 \handler (Event node) ->
  runEffectFn2 _subscribeNode handler node

globalEffectQueue :: Queue (Effect Unit)
globalEffectQueue = unsafePerformEffect Queue.new

_subscribeNode :: forall a. EffectFn2 (a -> Effect Unit) (Node a) Unsubscribe
_subscribeNode = mkEffectFn2 \handler node -> do
  let h = mkEffectFn1 \value -> do
            runEffectFn2 Queue.enqueue globalEffectQueue (handler value)
  runEffectFn2 I.addObserver node h
  pure (runEffectFn2 I.removeObserver node h)

drainEffects :: Effect Unit
drainEffects = runEffectFn2 Queue.drain globalEffectQueue (mkEffectFn1 \handler -> handler)

-- | Create an Event that can be triggered externally.
-- | Each `fire` will run a frame where the event occurs.
newEvent :: forall m a. MonadEffect m => m { event :: Event a, fire :: a -> Effect Unit }
newEvent = liftEffect do
  evt <- I.newEvent
  runEffectFn2 Node.annotate (I.readEvent evt) "root Event"
  pure
    { event: Event (I.readEvent evt)
    , fire: \x -> do
        runEffectFn2 I.triggerEvent evt x
        stabilize
    }

stabilize :: Effect Unit
stabilize = do
  I.stabilize
  drainEffects

-- | Create a new Behavior whose value can be modified outside a frame.
newBehavior :: forall m a. MonadEffect m => a -> m { behavior :: Behavior a, set :: a -> Effect Unit }
newBehavior initialValue = liftEffect $ newBehaviorEffect initialValue

newBehaviorEffect :: forall a. a -> Effect { behavior :: Behavior a, set :: a -> Effect Unit }
newBehaviorEffect initialValue = do
  { dynamic, set } <- newDynamic initialValue
  pure { behavior: Behavior dynamic, set }

-- | An Event that occurs when any of the events occur.
-- | If some of them occur simultaneously, the occurence value is that of the
-- | leftmost one.
leftmost :: forall a. Array (Event a) -> Event a
leftmost events = Event $ unsafePerformEffect do
  n <- runEffectFn1 I.leftmost (map (\(Event x) -> x) events)
  runEffectFn2 Node.annotate n "leftmost"
  pure n

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
newtype Dynamic a = Dynamic (Node a)

-- | The Behavior representing the current value of the Dynamic.
-- | When it is changing (the change event occurs), it has the new value.
-- |
-- | The value of `current x` is always the value of the latest occurence of
-- | `changed x`, if it has ever occured.
current :: forall a. Dynamic a -> Behavior a
current = Behavior

-- | An Event that fires with the new value every time the Dynamic changes.
changed :: forall a. Dynamic a -> Event a
changed (Dynamic node) = Event node

-- | An Event that fires every time the Dynamic changes.
changed_ :: forall a. Dynamic a -> Event Unit
changed_ = changed <<< void

instance functorDynamic :: Functor Dynamic where
  map f (Dynamic node) = Dynamic $ unsafePerformEffect do
    n <- runEffectFn2 I.map f node
    runEffectFn2 Node.annotate n "mapDynamic"
    pure n

instance applyDynamic :: Apply Dynamic where
  apply (Dynamic f) (Dynamic x) = Dynamic $ unsafePerformEffect do
    n <- runEffectFn3 I.map2 (mkFn2 ($)) f x
    runEffectFn2 Node.annotate n "applyDynamic"
    pure n

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
  n <- liftEffect do
    n <- runEffectFn3 I.fold (mkFn2 \a b -> Optional.some (f a b)) initial event
    runEffectFn2 Node.annotate n "foldDyn"
    pure n
  subscribeNode (\_ -> pure unit) n
  pure (Dynamic n)

effectCrash :: forall t304. String -> t304
effectCrash msg = unsafeCoerce ((\_ -> unsafeCrashWith msg) :: forall a. Unit -> a)

-- | Construct a new root Dynamic that can be changed from `Effect`-land.
newDynamic :: forall m a. MonadEffect m => a -> m { dynamic :: Dynamic a, read :: Effect a, set :: a -> Effect Unit, modify :: (a -> a) -> Effect Unit }
newDynamic initial = liftEffect do
  var <- runEffectFn1 I.newVar initial
  runEffectFn2 Node.annotate (I.readVar var) "root Dynamic"
  pure
    { dynamic: Dynamic (I.readVar var)
    , read: readNode (I.readVar var)
    , set: \x -> do
        runEffectFn2 I.setVar var x
        stabilize
    , modify: \f -> do
        x <- runEffectFn1 Node.valueExc (I.readVar var)
        runEffectFn2 I.setVar var (f x)
        stabilize
    }

-- | Like `foldDyn`, but the Dynamic will not update if the folding function
-- | returns Nothing.
foldDynMaybe :: forall m a b. MonadFRP m => (a -> b -> Maybe b) -> b -> Event a -> m (Dynamic b)
foldDynMaybe f initial (Event event) = do
  n <- liftEffect do
    n <- runEffectFn3 I.fold (mkFn2 \a b -> maybe Optional.none Optional.some (f a b)) initial event
    runEffectFn2 Node.annotate n "foldDynMaybe"
    pure n
  subscribeNode (\_ -> pure unit) n
  pure (Dynamic n)

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

uniqDyn :: forall m a. MonadFRP m => Eq a => Dynamic a -> m (Dynamic a)
uniqDyn = uniqDynBy (==)

-- | Make an Event that occurs when the current value of the given Dynamic (an Event) occurs.
switch :: forall a. Dynamic (Event a) -> Event a
switch (Dynamic lhs) = Event $ unsafePerformEffect do
  n <- runEffectFn3 I.switch false lhs (\(Event e) -> e)
  runEffectFn2 Node.annotate n "switch"
  pure n

instance bindDynamic :: Bind Dynamic where
  bind (Dynamic lhs) f = Dynamic $ unsafePerformEffect do
    n <- runEffectFn2 I.bind_ lhs (\x -> let Dynamic d = f x in d)
    runEffectFn2 Node.annotate n "bindDynamic"
    pure n

instance monadDynamic :: Monad Dynamic

subscribeDyn_
  :: forall m a
   . MonadFRP m
  => (a -> Effect Unit)
  -> Dynamic a
  -> m Unit
subscribeDyn_ handler dyn@(Dynamic node) = do
  subscribeNode handler node
  liftEffect do
    currentValue <- runEffectFn1 Node.valueExc node
    liftEffect $ handler currentValue

subscribeDyn ::
     forall m a b
   . MonadFRP m
  => (a -> Effect b)
  -> Dynamic a
  -> m (Dynamic b)
subscribeDyn handler dyn@(Dynamic node) = do
  evt <- liftEffect do
    evt <- I.newEvent
    runEffectFn2 Node.annotate (I.readEvent evt) "subscribeDyn"
    pure evt

  subscribeNode (\x -> do
                   value <- handler x
                   runEffectFn2 I.triggerEvent evt value
                   stabilize) node

  liftEffect do
    currentValue <- runEffectFn1 Node.valueExc node
    initialResult <- handler currentValue
    runEffectFn2 Node.set_value (I.readEvent evt) (Optional.some initialResult)
    pure (Dynamic (I.readEvent evt))

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
  currentValue <- readDynamic dyn
  foldDynMaybe (\new _ -> map Just new) currentValue (changed dyn)

readDynamic :: forall m a. MonadEffect m => Dynamic a -> m a
readDynamic = pull <<< readBehavior <<< current

-- | A "type class alias" for the constraints required by most FRP primitives.
class (MonadEffect m, MonadCleanup m) <= MonadFRP m
instance monadFRP :: (MonadEffect m, MonadCleanup m) => MonadFRP m


traceEventIO :: forall a. (a -> Effect Unit) -> Event a -> Event a
traceEventIO handler (Event n) = Event (traceNode handler n)

traceDynIO :: forall a. (a -> Effect Unit) -> Dynamic a -> Dynamic a
traceDynIO handler (Dynamic n) = Dynamic (traceNode handler n)


traceNode :: forall a. (a -> Effect Unit) -> Node a -> Node a
traceNode handler input = unsafePerformEffect do
  n <- runEffectFn2 I.traceChanges (mkEffectFn1 handler) input
  runEffectFn2 Node.annotate n "trace"
  pure n

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
