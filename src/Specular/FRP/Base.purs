module Specular.FRP.Base (
    Event
  , newEvent
  , subscribeEvent_
  , never
  , leftmost

  , Behavior
  , newBehavior

  , mergeEvents
  , sampleAt

  , filterMapEvent

  , Dynamic
  , current
  , changed
  , holdDyn
  , foldDyn
  , subscribeDyn_
  , switch
  , tagDyn
) where

import Prelude

import Control.Monad.Cleanup (class MonadCleanup, onCleanup)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.IOSync (IOSync, runIOSync)
import Control.Monad.IOSync.Class (class MonadIOSync, liftIOSync)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Foldable (for_, sequence_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Data.UniqueMap.Mutable as UMM
import Partial.Unsafe (unsafeCrashWith)
import Data.Array as Array

type FrameEnv = { currentTime :: Time, effectsRef :: IORef (IOSync Unit) }

-- | Computations that occur during a Frame.
--
-- During a frame, no arbitrary effects are performed. Instead they are
-- registered using `effect` to be performed after the frame.
--
-- Frame computations have access to current logical time. See `oncePerFrame`
-- for why this is needed.
newtype Frame a = Frame (ReaderT FrameEnv IOSync a)

getTime :: Frame Time
getTime = Frame $ asks _.currentTime

-- | Schedule an effect to be executed after the Frame completed.
effect :: IOSync Unit -> Frame Unit
effect action = Frame $ do
  effectsRef <- asks _.effectsRef
  liftIOSync $ modifyIORef effectsRef (_ *> action)

derive newtype instance functorFrame :: Functor Frame
derive newtype instance applyFrame :: Apply Frame
derive newtype instance applicativeFrame :: Applicative Frame
derive newtype instance bindFrame :: Bind Frame
derive newtype instance monadFrame :: Monad Frame

internalFrameIOSync :: forall a. IOSync a -> Frame a
internalFrameIOSync = Frame <<< liftIOSync

-- | Run a Frame computation and then run its effects.
runFrame :: forall a. Time -> Frame a -> IOSync a
runFrame currentTime (Frame x) = do
  effectsRef <- newIORef (pure unit)
  value <- runReaderT x { currentTime, effectsRef }
  join $ readIORef effectsRef
  pure value

-- | Run a Frame computation with a fresh time value and then run its effects.
runNextFrame :: forall a. Frame a -> IOSync a
runNextFrame frame = do
  time <- readIORef nextTimeRef
  writeIORef nextTimeRef (case time of Time t -> Time (t + 1))
  runFrame time frame

-- | Create a computation that will run the given action at most once during
-- each Frame. if `x <- oncePerFrame action`, then `x *> x = x`.
oncePerFrame_ :: Frame Unit -> IOSync (Frame Unit)
oncePerFrame_ action = do
  ref <- newIORef Nothing
  pure $ do
    time <- getTime
    m_lastTime <- internalFrameIOSync $ readIORef ref
    case m_lastTime of
      Just lastTime | lastTime == time ->
        pure unit
      _ -> do
        internalFrameIOSync $ writeIORef ref (Just time)
        action

data CacheState a =
    Fresh
  | Cached Time a
  | BlackHole

-- | Create a computation that will run the given action at most once during
-- each Frame. if `x <- oncePerFrame action`, then `x *> x = x`.
--
-- It is an error to run the computation returned from `oncePerFrame` inside
-- the passed action.
oncePerFrame :: forall a. Frame a -> IOSync (Frame a)
oncePerFrame action = do
  ref <- newIORef Fresh
  pure $ do
    time <- getTime
    cache <- internalFrameIOSync $ readIORef ref
    case cache of
      Cached lastTime value | lastTime == time ->
        pure value

      BlackHole ->
        unsafeCrashWith "Illegal self-referential computation passed to oncePerFrame"

      _ -> do
        value <- action
        internalFrameIOSync $ writeIORef ref (Cached time value)
        pure value

-------------------------------------------------------------

-- | Logical time.
-- There's no monotonicity requirement (for now), so we have only Eq instance.
newtype Time = Time Int

derive newtype instance eqTime :: Eq Time

-- | The global time counter.
nextTimeRef :: IORef Time
nextTimeRef = unsafePerformEff $ runIOSync $ newIORef (Time 0)

-------------------------------------------------------------

-- | Behaviors are time-changing values that can be read, but not subscribed to.
--
-- A Behavior should never change during a frame.
--
-- Can be composed using Monad instance.
newtype Behavior a = Behavior (Frame a)
-- Behavior is represented by a computation that reads its value.

-- | Create a new Behavior whose value can be modified outside a frame.
newBehavior :: forall a. a -> IOSync { behavior :: Behavior a, set :: a -> IOSync Unit }
newBehavior initialValue = do
  ref <- newIORef initialValue
  pure
    { behavior: Behavior $ internalFrameIOSync $ readIORef ref
    , set: writeIORef ref
    }

-- | Read a value of a Behavior.
readBehavior :: forall a. Behavior a -> Frame a
readBehavior (Behavior read) = read

instance functorBehavior :: Functor Behavior where
  map f (Behavior read) = Behavior $ map f read

instance applyBehavior :: Apply Behavior where
  apply (Behavior f) (Behavior x) = Behavior $ f <*> x

instance applicativeBehavior :: Applicative Behavior where
  pure x = Behavior $ pure x

instance bindBehavior :: Bind Behavior where
  bind (Behavior read) k = Behavior $ do
    value <- read
    readBehavior (k value)

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

-- | Create an Event that can be triggered externally.
-- | Each `fire` will run a frame where the event occurs.
newEvent :: forall a. IOSync { event :: Event a, fire :: a -> IOSync Unit }
newEvent = do
  occurence <- newBehavior Nothing
  listenerMap <- UMM.new
  let
    fire value = do
      occurence.set (Just value)
      listeners <- UMM.values listenerMap
      runNextFrame $ sequence_ listeners
      occurence.set Nothing -- FIXME: this should occur before effects of the frame,
                            -- because they may run other frames

    subscribe l = do
      key <- UMM.insert l listenerMap
      pure $ UMM.delete key listenerMap

  pure
    { event: Event { occurence: occurence.behavior, subscribe }
    , fire
    }

instance functorEvent :: Functor Event where
  map f (Event {occurence, subscribe}) = Event { occurence: map (map f) occurence, subscribe }

-- | An Event that never occurs.
never :: forall a. Event a
never = Event { occurence: pure Nothing, subscribe: \_ -> pure (pure unit) }

filterMapEventB :: forall a b. (a -> Behavior (Maybe b)) -> Event a -> Event b
filterMapEventB f (Event {occurence, subscribe}) =
  Event
    { occurence: (map join <<< join <<< map sequence) (map (map f) occurence)
    , subscribe
    }

mapEventB :: forall a b. (a -> Behavior b) -> Event a -> Event b
mapEventB f (Event {occurence, subscribe}) =
  Event
    { occurence: (join <<< map sequence) (map (map f) occurence)
    , subscribe
    }

sampleAt :: forall a b. Event (a -> b) -> Behavior a -> Event b
sampleAt event behavior = mapEventB (\f -> f <$> behavior) event

filterMapEvent :: forall a b. (a -> Maybe b) -> Event a -> Event b
filterMapEvent f = filterMapEventB (pure <<< f)

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

subscribeEvent_ ::
     forall m a
   . MonadCleanup m
  => MonadIOSync m
  => (a -> IOSync Unit)
  -> Event a
  -> m Unit
subscribeEvent_ handler (Event {occurence,subscribe}) = do
  unsub <- liftIOSync $ subscribe $ do
    m_value <- readBehavior occurence
    for_ m_value $ \value ->
      effect $ handler value
  onCleanup unsub

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
       pure $ sequence_ unsubs
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

newtype Dynamic a = Dynamic
  { value :: Behavior a
  , change :: Event Unit
  }

holdDyn ::
     forall m a
   . MonadCleanup m
  => MonadIOSync m
  => a
  -> Event a
  -> m (Dynamic a)
holdDyn = foldDyn (\x _ -> x)

foldDyn ::
     forall m a b
   . MonadCleanup m
  => MonadIOSync m
  => (a -> b -> b)
  -> b
  -> Event a
  -> m (Dynamic b)
foldDyn f initial (Event event) = do
  ref <- liftIOSync $ newIORef initial
  updateOrReadValue <- liftIOSync $ oncePerFrame $ do
    m_newValue <- readBehavior event.occurence
    internalFrameIOSync $ do
      oldValue <- readIORef ref
      case m_newValue of
        Just occurence -> do
          let newValue = f occurence oldValue
          writeIORef ref newValue
          pure newValue
        Nothing ->
          pure oldValue

  unsub <- liftIOSync $ event.subscribe $ void $ updateOrReadValue
  onCleanup unsub

  pure $ Dynamic
    { value: Behavior updateOrReadValue
    , change: map (\_ -> unit) (Event event)
    }

current :: forall a. Dynamic a -> Behavior a
current (Dynamic {value}) = value

changed :: forall a. Dynamic a -> Event a
changed (Dynamic {value, change}) = mapEventB (\_ -> value) change

instance functorDynamic :: Functor Dynamic where
  map f (Dynamic { value, change }) = Dynamic { value: map f value, change }

instance applyDynamic :: Apply Dynamic where
  apply (Dynamic f) (Dynamic x) = Dynamic
    { value: f.value <*> x.value
    , change: mergePulses f.change x.change
    }

instance applicativeDynamic :: Applicative Dynamic where
  pure x = Dynamic { value: pure x, change: never }

-- | Make an Event that occurs when the current value of the given Dynamic (an Event) occurs.
switch :: forall a. Dynamic (Event a) -> Event a
switch (Dynamic { value, change: Event change }) = Event
  { occurence: do
      -- The resulting Event occurs when the current inner Event occurs
      Event innerEvent <- value
      innerEvent.occurence

  , subscribe: \l -> do
      onceListener <- oncePerFrame l
      -- oncePerFrame guards us against the case of coincidence
      -- of the inner Event and outer Dynamic change

      unsubRef <- newIORef (pure unit)

      let
        cleanup :: IOSync Unit
        cleanup = join (readIORef unsubRef)

        replaceWith :: Event a -> Frame Unit
        replaceWith (Event event) = do
          effect $ do
            cleanup
            unsub <- event.subscribe onceListener
            writeIORef unsubRef unsub

        -- Unsubscribe from the previous change event (if any)
        -- and subscribe to the current one.
        updateListener :: Frame Unit
        updateListener = readBehavior value >>= replaceWith

      -- First, we subscribe to the current inner Dynamic
      runNextFrame updateListener

      unsub <- change.subscribe $ do
        -- when the outer Dynamic changes,

        onceListener
        -- we notify our listener

        updateListener
        -- and resubscribe

      pure (cleanup *> unsub)
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

subscribeDyn_ ::
     forall m a
   . MonadCleanup m
  => MonadIOSync m
  => (a -> IOSync Unit)
  -> Dynamic a
  -> m Unit
subscribeDyn_ handler (Dynamic {value, change}) = do
  liftIOSync $ runNextFrame (readBehavior value) >>= handler
  subscribeEvent_ handler (mapEventB (\_ -> value) change)

tagDyn :: forall a. Dynamic a -> Event Unit -> Event a
tagDyn dyn event = sampleAt (id <$ event) (current dyn)
