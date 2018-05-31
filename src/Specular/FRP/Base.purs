module Specular.FRP.Base (
    Event
  , never
  , leftmost
  , mergeEvents

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
  , hostEffect

  , class MonadFRP

  , for

  , subscribeEvent_Impl
  , foldDynImpl
  , foldDynMaybeImpl
) where

import Prelude

import Control.Monad.Cleanup (class MonadCleanup, onCleanup)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.IOSync (IOSync, runIOSync)
import Control.Monad.IOSync.Class (class MonadIOSync, liftIOSync)
import Control.Monad.Reader (ask)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (sequence, traverse)
import Partial.Unsafe (unsafeCrashWith)
import Specular.Internal.Effect (DelayedEffects, Ref, emptyDelayed, newRef, pushDelayed, readRef, sequenceEffects, unsafeFreezeDelayed, writeRef)
import Specular.Internal.RIO (RIO, rio, runRIO)
import Specular.Internal.RIO as RIO
import Specular.Internal.UniqueMap.Mutable as UMM

-------------------------------------------------

-- | Pull is a computation that reads a value given current time.
-- |
-- | Invariant: Pull computations are always idempotent (`forall x :: Pull a. x *> x = x`).
newtype Pull a = MkPull (RIO Time a)

runPull :: forall a. Time -> Pull a -> IOSync a
runPull time (MkPull x) = runRIO time x

derive newtype instance functorPull :: Functor Pull
derive newtype instance applyPull :: Apply Pull
derive newtype instance applicativePull :: Applicative Pull
derive newtype instance bindPull :: Bind Pull
derive newtype instance monadPull :: Monad Pull

getTime :: Pull Time
getTime =
  -- ask is idempotent
  MkPull ask

pullReadRef :: forall a. Ref a -> Pull a
pullReadRef ref =
  -- readRef is idempotent
  MkPull (liftIOSync (readRef ref))

unsafeMkPull :: forall a. (Time -> IOSync a) -> Pull a
unsafeMkPull f = MkPull (rio f)

data CacheState a =
    Fresh
  | Cached Time a
  | BlackHole

-- | Create a computation that will run the given action at most once during
-- | each frame.
-- |
-- | It is an error to run the computation returned from `oncePerFrame` inside
-- | the passed action.
oncePerFramePull :: forall a. Pull a -> IOSync (Pull a)
oncePerFramePull action = oncePerFramePullWithIO action pure

-- | Create a computation that will run the given Pull action and chain it to
-- | the given IO action, at most once per frame. This makes the returned Pull
-- | idempotent by construction.
-- |
-- | It is an error to run the computation returned from `oncePerFrame` inside
-- | the passed action.
oncePerFramePullWithIO :: forall a b. Pull a -> (a -> IOSync b) -> IOSync (Pull b)
oncePerFramePullWithIO action io = do
  ref <- newRef Fresh
  pure $ unsafeMkPull $ \time -> do
    cache <- readRef ref
    case cache of
      Cached lastTime value | lastTime == time ->
        pure value

      BlackHole ->
        unsafeCrashWith "Illegal self-referential computation passed to oncePerFrame"

      _ -> do
        writeRef ref BlackHole
        value <- runPull time action >>= io
        writeRef ref (Cached time value)
        pure value

pull :: forall m a. MonadIOSync m => Pull a -> m a
pull p = liftIOSync do
  time <- freshTime
  runPull time p

-------------------------------------------------

-- | Computations that occur during a Frame.
--
-- During a frame, no arbitrary effects are performed. Instead they are
-- registered using `effect` to be performed after the frame.
--
-- Frame computations have access to current logical time. See `oncePerFrame`
-- for why this is needed.
newtype Frame a = Frame (RIO FrameEnv a)

type FrameEnv =
  { effects :: DelayedEffects
  , time :: Time
  }

framePull :: forall a. Pull a -> Frame a
framePull (MkPull x) = Frame (RIO.local _.time x)

frameWriteRef :: forall a. Ref a -> a -> Frame Unit
frameWriteRef ref value = Frame (liftIOSync (writeRef ref value))

-- | Schedule an effect to be executed after the Frame completed.
effect :: IOSync Unit -> Frame Unit
effect action = Frame $ rio $ \{effects} ->
  -- HACK: briefly creating a Pull that is not idempotent;
  -- But it's immediately lifted to Frame, so it's OK
  void $ pushDelayed effects action

derive newtype instance functorFrame :: Functor Frame
derive newtype instance applyFrame :: Apply Frame
derive newtype instance applicativeFrame :: Applicative Frame
derive newtype instance bindFrame :: Bind Frame
derive newtype instance monadFrame :: Monad Frame

-- | Run a Frame computation and then run its effects.
runFrame :: forall a. Time -> Frame a -> IOSync a
runFrame currentTime (Frame x) = do
  effectsMutable <- emptyDelayed
  value <- runRIO { effects: effectsMutable, time: currentTime } x
  effects <- unsafeFreezeDelayed effectsMutable
  sequenceEffects effects
  pure value

freshTime :: IOSync Time
freshTime = do
  time <- readRef nextTimeRef
  writeRef nextTimeRef (case time of Time t -> Time (t + 1))
  pure time

-- | Run a Frame computation with a fresh time value and then run its effects.
runNextFrame :: forall a. Frame a -> IOSync a
runNextFrame frame = do
  time <- freshTime
  runFrame time frame

-- | Create a computation that will run the given action at most once during
-- | each Frame. if `x <- oncePerFrame_ action`, then `x *> x = x`.
oncePerFrame_ :: Frame Unit -> IOSync (Frame Unit)
oncePerFrame_ action = do
  ref <- newRef Nothing
  pure $ do
    time <- framePull $ getTime
    m_lastTime <- framePull $ pullReadRef ref
    case m_lastTime of
      Just lastTime | lastTime == time ->
        pure unit
      _ -> do
        frameWriteRef ref (Just time)
        action

-------------------------------------------------------------

-- | Logical time.
-- There's no monotonicity requirement (for now), so we have only Eq instance.
newtype Time = Time Int

derive newtype instance eqTime :: Eq Time

-- | The global time counter.
nextTimeRef :: Ref Time
nextTimeRef = unsafePerformEff $ runIOSync $ newRef (Time 0)

-------------------------------------------------------------

-- | Behaviors are time-changing values that can be read, but not subscribed to.
--
-- A Behavior should never change during a frame.
--
-- Can be composed using Monad instance.
newtype Behavior a = Behavior (Pull a)
-- Behavior is represented by a computation that reads its value.

-- | Read a value of a Behavior.
readBehavior :: forall a. Behavior a -> Pull a
readBehavior (Behavior read) = read

derive newtype instance functorBehavior :: Functor Behavior
derive newtype instance applyBehavior :: Apply Behavior
derive newtype instance applicativeBehavior :: Applicative Behavior
derive newtype instance bindBehavior :: Bind Behavior
instance monadBehavior :: Monad Behavior

-------------------------------------------------------------

type Listener = Frame Unit
type Unsubscribe = IOSync Unit

-- | A source of occurences.
-- |
-- | During a frame, an Event occurs at most once with a value of type a.
-- | 
-- | Event is a functor. It is not, however, an Applicative. There is no
-- | meaningful interpretation of `pure` (when would the event occur?).
-- | There is an interpretation of `apply` (Event that fires when the input
-- | events coincide), but it's not very useful.
newtype Event a = Event
  { occurence :: Behavior (Maybe a)
  , subscribe :: Listener -> IOSync Unsubscribe
  }
-- We represent an Event with:
--  - a Behavior that tells whether this Event occurs during a given frame,
--    and if so, its occurence value,
--  - subscription function.

derive instance functorEvent :: Functor Event

-- | An Event that never occurs.
never :: forall a. Event a
never = Event { occurence: pure Nothing, subscribe: \_ -> pure (pure unit) }

filterMapEventB :: forall a b. (a -> Behavior (Maybe b)) -> Event a -> Event b
filterMapEventB f (Event {occurence, subscribe}) = gated $
  Event
    { occurence: (map join <<< join <<< map sequence) (map (map f) occurence)
    , subscribe
    }

mapEventB :: forall a b. (a -> Behavior b) -> Event a -> Event b
mapEventB f (Event {occurence, subscribe}) = gated $
  Event
    { occurence: (join <<< map sequence) (map (map f) occurence)
    , subscribe
    }

gated :: forall a. Event a -> Event a
gated (Event {occurence, subscribe}) =
  Event
    { occurence
    , subscribe: \l ->
        subscribe $ do
          occ <- framePull $ readBehavior occurence
          when (isJust occ) l
    }

sampleAt :: forall a b. Event (a -> b) -> Behavior a -> Event b
sampleAt event behavior = mapEventB (\f -> f <$> behavior) event

filterMapEvent :: forall a b. (a -> Maybe b) -> Event a -> Event b
filterMapEvent f = filterMapEventB (pure <<< f)

filterEvent :: forall a. (a -> Boolean) -> Event a -> Event a
filterEvent f = filterMapEvent (\x -> if f x then Just x else Nothing)

mergeEvents ::
     forall a b c
   . (a -> Behavior c)
  -> (b -> Behavior c)
  -> (a -> b -> Behavior c)
  -> Event a
  -> Event b
  -> Event c
mergeEvents whenLeft whenRight whenBoth (Event left) (Event right) =
  Event
    { occurence: do
        occL <- left.occurence
        occR <- right.occurence
        case occL, occR of
          Just l,  Nothing -> Just <$> whenLeft l
          Nothing, Just r  -> Just <$> whenRight r
          Just l,  Just r  -> Just <$> whenBoth l r
          Nothing, Nothing -> pure Nothing
    , subscribe: \l -> do
       onceListener <- oncePerFrame_ l
       unsubL <- left.subscribe onceListener
       unsubR <- right.subscribe onceListener
       pure $ unsubL *> unsubR
    }

mergePulses :: Event Unit -> Event Unit -> Event Unit
mergePulses = mergeEvents (\_ -> pure unit) (\_ -> pure unit) (\_ _ -> pure unit)

subscribeEvent_ :: forall m a. MonadIOSync m => MonadCleanup m => (a -> IOSync Unit) -> Event a -> m Unit
subscribeEvent_ = subscribeEvent_Impl

subscribeEvent_Impl :: forall m a. MonadCleanup m => MonadIOSync m => (a -> IOSync Unit) -> Event a -> m Unit
subscribeEvent_Impl handler (Event {occurence,subscribe}) = do
  unsub <- liftIOSync $ subscribe $ do
    m_value <- framePull $ readBehavior occurence
    for_ m_value $ \value ->
      effect $ handler value
  onCleanup unsub

-- | DEPRECATED: Replace this with `liftIOSync`.
hostEffect :: forall m a. MonadIOSync m => IOSync a -> m a
hostEffect = liftIOSync

-- | Create an Event that can be triggered externally.
-- | Each `fire` will run a frame where the event occurs.
newEvent :: forall m a. MonadIOSync m => m { event :: Event a, fire :: a -> IOSync Unit }
newEvent = liftIOSync do
  occurenceRef <- newRef Nothing
  listenerMap <- UMM.new
  let
    fire value = do
      writeRef occurenceRef (Just value)
      listeners <- UMM.values listenerMap
      runNextFrame $ do
        sequenceFrame_ listeners
        frameWriteRef occurenceRef Nothing

    subscribe l = do
      key <- UMM.insert l listenerMap
      pure $ UMM.delete key listenerMap

  pure
    { event: Event { occurence: Behavior $ pullReadRef occurenceRef, subscribe }
    , fire
    }

-- | Create a new Behavior whose value can be modified outside a frame.
newBehavior :: forall m a. MonadIOSync m => a -> m { behavior :: Behavior a, set :: a -> IOSync Unit }
newBehavior initialValue = liftIOSync $ newBehaviorIOSync initialValue

foreign import sequenceFrame_ :: Array (Frame Unit) -> Frame Unit

newBehaviorIOSync :: forall a. a -> IOSync { behavior :: Behavior a, set :: a -> IOSync Unit }
newBehaviorIOSync initialValue = do
  ref <- newRef initialValue
  pure
    { behavior: Behavior $ pullReadRef ref
    , set: writeRef ref
    }

-- | An Event that occurs when any of the events occur.
-- | If some of them occur simultaneously, the occurence value is that of the
-- | leftmost one.
leftmost :: forall a. Array (Event a) -> Event a
leftmost events =
  Event
    { occurence: findFirstM (\(Event event) -> event.occurence) events
    , subscribe: \l -> do
       onceListener <- oncePerFrame_ l
       unsubs <- traverse (\(Event event) -> event.subscribe onceListener) events
       pure $ sequenceEffects unsubs
    }

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
--
-- Dynamic is implemented as a pair of `Behavior` and `Event`. The Event only
-- notifies when a change occurs; the new value is always queried from the
-- behavior.
newtype Dynamic a = Dynamic
  { value :: Behavior a
  , change :: Event Unit
  }

-- | The Behavior representing the current value of the Dynamic.
-- | When it is changing (the change event occurs), it has the new value.
-- |
-- | The value of `current x` is always the value of the latest occurence of
-- | `changed x`, if it has ever occured.
current :: forall a. Dynamic a -> Behavior a
current (Dynamic {value}) = value

-- | An Event that fires with the new value every time the Dynamic changes.
changed :: forall a. Dynamic a -> Event a
changed (Dynamic {value, change}) = mapEventB (\_ -> value) change

-- | An Event that fires every time the Dynamic changes.
changed_ :: forall a. Dynamic a -> Event Unit
changed_ (Dynamic {value, change}) = change

derive instance functorDynamic :: Functor Dynamic

instance applyDynamic :: Apply Dynamic where
  apply (Dynamic f) (Dynamic x) = Dynamic
    { value: f.value <*> x.value
    , change: mergePulses f.change x.change
    }

instance applicativeDynamic :: Applicative Dynamic where
  pure x = Dynamic { value: pure x, change: never }

-- | `foldDyn f x e` - Make a Dynamic that will have the initial value `x`,
-- | and every time `e` fires, its value will update by applying `f` to the
-- | event occurence value and the old value.
-- |
-- | On cleanup, the Dynamic will stop updating in response to the event.
foldDyn :: forall m a b. MonadFRP m => (a -> b -> b) -> b -> Event a -> m (Dynamic b)
foldDyn = foldDynImpl

foldDynImpl
  :: forall m a b. MonadCleanup m => MonadIOSync m
  => (a -> b -> b) -> b -> Event a -> m (Dynamic b)
foldDynImpl f initial (Event event) = do
  ref <- liftIOSync $ newRef initial
  updateOrReadValue <- liftIOSync $
    oncePerFramePullWithIO (readBehavior event.occurence) $ \m_newValue -> do
      oldValue <- readRef ref
      case m_newValue of
        Just occurence -> do
          let newValue = f occurence oldValue
          writeRef ref newValue
          pure newValue
        Nothing ->
          pure oldValue

  unsub <- liftIOSync $ event.subscribe $ void $ framePull $ updateOrReadValue
  onCleanup unsub

  pure $ Dynamic
    { value: Behavior updateOrReadValue
    , change: map (\_ -> unit) (Event event)
    }

-- | Like `foldDyn`, but the Dynamic will not update if the folding function
-- | returns Nothing.
foldDynMaybe :: forall m a b. MonadFRP m => (a -> b -> Maybe b) -> b -> Event a -> m (Dynamic b)
foldDynMaybe = foldDynMaybeImpl

foldDynMaybeImpl
  :: forall m a b. MonadCleanup m => MonadIOSync m
   => (a -> b -> Maybe b) -> b -> Event a -> m (Dynamic b)
foldDynMaybeImpl f initial (Event event) = do
  ref <- liftIOSync $ newRef initial
  (updateOrReadValue :: Pull { changing :: Boolean, value :: b }) <- liftIOSync $
    oncePerFramePullWithIO (readBehavior event.occurence) $ \m_newValue -> do
      oldValue <- readRef ref
      case m_newValue of
        Just occurence | Just newValue <- f occurence oldValue -> do
          writeRef ref newValue
          pure { changing: true, value: newValue }
        _ ->
          pure { changing: false, value: oldValue }

  unsub <- liftIOSync $ event.subscribe $ void $ framePull $ updateOrReadValue
  onCleanup unsub

  pure $ Dynamic
    { value: Behavior $ map _.value updateOrReadValue
    , change: filterMapEventB (\_ ->
                                 map (\{changing} ->
                                        if changing then Just unit else Nothing)
                                     (Behavior updateOrReadValue))
                              (Event event)
    }

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
switch (Dynamic { value, change: Event change }) = Event
  { occurence: do
      -- The resulting Event occurs when the current inner Event occurs
      Event innerEvent <- value
      innerEvent.occurence

  , subscribe: \l -> do
      onceListener <- oncePerFrame_ l
      -- oncePerFrame guards us against the case of coincidence
      -- of the inner Event and outer Dynamic change

      unsubRef <- newRef (pure unit)
      isDoneRef <- newRef false

      let
        cleanup :: IOSync Unit
        cleanup = join (readRef unsubRef)

        replaceWith :: Event a -> Frame Unit
        replaceWith (Event event) = do
          effect $ do
            isDone <- readRef isDoneRef
            unless isDone do
              cleanup
              unsub <- event.subscribe onceListener
              writeRef unsubRef unsub

        -- Unsubscribe from the previous change event (if any)
        -- and subscribe to the current one.
        updateListener :: Frame Unit
        updateListener = framePull (readBehavior value) >>= replaceWith

      -- First, we subscribe to the current inner Dynamic
      runNextFrame updateListener

      unsub <- change.subscribe $ do
        -- when the outer Dynamic changes,

        onceListener
        -- we notify our listener

        updateListener
        -- and resubscribe

      pure (writeRef isDoneRef true *> cleanup *> unsub)
  }


joinDyn :: forall a. Dynamic (Dynamic a) -> Dynamic a
joinDyn dynamic@(Dynamic {change}) = Dynamic
  { value: current dynamic >>= current
      -- Value is the current value of the inner Dynamic
  , change:
      -- The resulting Dynamic changes when either:

      switch (map (\(Dynamic inner) -> inner.change) dynamic)
      --  - the inner Dynamic changes,

      `mergePulses` change
      --  - the outer Dynamic changes.
  }

instance bindDynamic :: Bind Dynamic where
  bind d f = joinDyn (map f d)

instance monadDynamic :: Monad Dynamic

subscribeDyn_
  :: forall m a
   . MonadFRP m
  => (a -> IOSync Unit)
  -> Dynamic a
  -> m Unit
subscribeDyn_ handler (Dynamic {value, change}) = do
  currentValue <- pull $ readBehavior value
  liftIOSync $ handler currentValue
  subscribeEvent_ handler (mapEventB (\_ -> value) change)

subscribeDyn ::
     forall m a b
   . MonadFRP m
  => (a -> IOSync b)
  -> Dynamic a
  -> m (Dynamic b)
subscribeDyn handler dyn = do
  {event,fire} <- newEvent
  currentValue <- pull $ readBehavior $ current dyn
  initialResult <- hostEffect $ handler currentValue
  subscribeEvent_ (handler >=> fire) $ changed dyn
  holdDyn initialResult event

tagDyn :: forall a. Dynamic a -> Event Unit -> Event a
tagDyn dyn event = sampleAt (id <$ event) (current dyn)

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

-- | A "type class alias" for the constraints required by most FRP primitives.
class (MonadIOSync m, MonadCleanup m) <= MonadFRP m
instance monadFRP :: (MonadIOSync m, MonadCleanup m) => MonadFRP m

-- | Flipped `map`.
-- |
-- | Useful in conjunction with `dynamic`: Instead of `dynamic $ map (\x -> longExpression x) dyn`,
-- | you can write `dynamic $ for dyn $ \x -> longExpression x`.
-- TODO: This should be moved somewhere
for :: forall f a b. Functor f => f a -> (a -> b) -> f b
for = flip map
