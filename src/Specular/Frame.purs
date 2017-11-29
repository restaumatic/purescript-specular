module Specular.Frame (
    Time
  , Frame

  , Event
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

import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.IOSync (IOSync, runIOSync)
import Control.Monad.IOSync.Class (liftIOSync)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Foldable (for_, sequence_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.UniqueMap.Mutable as UMM

newtype Time = Time Int

derive newtype instance eqTime :: Eq Time

type FrameEnv = { currentTime :: Time, effectsRef :: IORef (IOSync Unit) }

newtype Frame a = Frame (ReaderT FrameEnv IOSync a)

getTime :: Frame Time
getTime = Frame $ asks _.currentTime

runFrame :: forall a. Time -> Frame a -> IOSync a
runFrame currentTime (Frame x) = do
  effectsRef <- newIORef (pure unit)
  value <- runReaderT x { currentTime, effectsRef }
  join $ readIORef effectsRef
  pure value

-- | Schedule an effect to be executed after the Frame runs.
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

-------------------------------------------------------------

nextTimeRef :: IORef Time
nextTimeRef = unsafePerformEff $ runIOSync $ newIORef (Time 0)

runNextFrame :: forall a. Frame a -> IOSync a
runNextFrame frame = do
  time <- readIORef nextTimeRef
  writeIORef nextTimeRef (case time of Time t -> Time (t + 1))
  runFrame time frame

-------------------------------------------------------------

newtype Behavior a = Behavior (Frame a)

newBehavior :: forall a. a -> IOSync { behavior :: Behavior a, set :: a -> IOSync Unit }
newBehavior initialValue = do
  ref <- newIORef initialValue
  pure
    { behavior: Behavior $ internalFrameIOSync $ readIORef ref
    , set: writeIORef ref
    }

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

newtype Event a = Event
  { occurence :: Behavior (Maybe a)
  , subscribe :: Listener -> IOSync Unsubscribe
  }

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

subscribeEvent_ :: forall a. (a -> IOSync Unit) -> Event a -> IOSync Unsubscribe
subscribeEvent_ handler (Event {occurence,subscribe}) =
  subscribe $ do
    m_value <- readBehavior occurence
    for_ m_value $ \value ->
      effect $ handler value

-----------------------------------------------------------------

newtype Dynamic a = Dynamic
  { value :: Behavior a
  , change :: Event Unit
  }

holdDyn :: forall a. a -> Event a -> IOSync (Dynamic a)
holdDyn initial (Event event) = do
  ref <- newIORef initial
  let value = Behavior $ do
        m_newValue <- readBehavior event.occurence
        internalFrameIOSync $ case m_newValue of
          Just newValue -> do
            writeIORef ref newValue
            pure newValue
          Nothing ->
            readIORef ref

  pure $ Dynamic { value, change: map (\_ -> unit) (Event event) }

subscribeDyn_ :: forall a. (a -> IOSync Unit) -> Dynamic a -> IOSync Unsubscribe
subscribeDyn_ handler (Dynamic {value, change}) = do
  runNextFrame (readBehavior value) >>= handler
  subscribeEvent_ handler (mapEventB (\_ -> value) change)
