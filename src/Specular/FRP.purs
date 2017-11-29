module Specular.FRP (
    Event
  , newEvent
  , subscribeEvent_

  , Behavior
  , newBehavior

  , mergeEvents
  , sampleAt

  , filterMapEvent

  , Dynamic
  , holdDyn
  , subscribeDyn_
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
import Data.Traversable (sequence)
import Data.UniqueMap.Mutable as UMM

type FrameEnv = { currentTime :: Time, effectsRef :: IORef (IOSync Unit) }

-- | Computations that occur during a Frame.
-- 
-- During a Frame, no arbitrary effects are performed. Instead they are
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
oncePerFrame :: Frame Unit -> IOSync (Frame Unit)
oncePerFrame action = do
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

-------------------------------------------------------------

type Listener = Frame Unit
type Unsubscribe = IOSync Unit

-- | A source of occurences.
--
-- During a frame, an Event occurs at most once with a value of type a.
--
-- Event is a functor. It is not, however, an Applicative. There is no
-- meaningful interpretation of `pure` (when would the event occur?).
-- There is an interpretation of `apply` (Event that fires when the input
-- events coincide), but it's not very useful.
newtype Event a = Event
  { occurence :: Behavior (Maybe a)
  , subscribe :: Listener -> IOSync Unsubscribe
  }
-- We represent an Event with:
--  - a Behavior that tells whether this Event occurs during a given frame,
--    and if so, its occurence value,
--  - subscription function.

-- | Create an Event that can be triggered externally.
-- Each `fire` will run a frame where the event occurs.
newEvent :: forall a. IOSync { event :: Event a, fire :: a -> IOSync Unit }
newEvent = do
  occurence <- newBehavior Nothing
  listenerMap <- UMM.new
  let
    fire value = do
      occurence.set (Just value)
      listeners <- UMM.values listenerMap
      runNextFrame $ sequence_ listeners
      occurence.set Nothing

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
       onceListener <- oncePerFrame l
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
holdDyn initial (Event event) = do
  ref <- liftIOSync $ newIORef initial
  let
    updateOrReadValue = do
      m_newValue <- readBehavior event.occurence
      internalFrameIOSync $ case m_newValue of
        Just newValue -> do
          writeIORef ref newValue
          pure newValue
        Nothing ->
          readIORef ref

  unsub <- liftIOSync $ event.subscribe $ void $ updateOrReadValue
  onCleanup unsub

  pure $ Dynamic
    { value: Behavior updateOrReadValue
    , change: map (\_ -> unit) (Event event)
    }

instance functorDynamic :: Functor Dynamic where
  map f (Dynamic { value, change }) = Dynamic { value: map f value, change }

instance applyDynamic :: Apply Dynamic where
  apply (Dynamic f) (Dynamic x) = Dynamic
    { value: f.value <*> x.value
    , change: mergePulses f.change x.change
    }

instance applicativeDynamic :: Applicative Dynamic where
  pure x = Dynamic { value: pure x, change: never }

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
