module Specular.FRP.Base (
    Event
  , never
  , leftmost
  , mergeEvents

  , filterEvent
  , filterMapEvent

  , Pull
  , class MonadPull
  , pull

  , Behavior
  , sampleAt
  , readBehavior

  , Dynamic
  , current
  , changed
  , switch
  , tagDyn
  , attachDynWith
  , latestJust

  , class MonadHold
  , holdDyn
  , foldDyn
  , foldDynMaybe
  , holdUniqDynBy

  , class MonadHostCreate
  , newEvent
  , newBehavior

  , class MonadHost
  , subscribeEvent_
  , subscribeDyn_
  , subscribeDyn
  , hostEffect

  , class MonadFRP

  , for
) where

import Prelude

import Control.Monad.Cleanup (class MonadCleanup, CleanupT, onCleanup)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.IOSync (IOSync, runIOSync)
import Control.Monad.IOSync.Class (class MonadIOSync, liftIOSync)
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.Reader.Trans (ReaderT(..))
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.DelayedEffects (DelayedEffects, sequenceEffects)
import Data.DelayedEffects as DE
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (sequence, traverse)
import Data.UniqueMap.Mutable as UMM
import Partial.Unsafe (unsafeCrashWith)

-------------------------------------------------

-- | Pull is a computation that reads a value given current time.
-- |
-- | Invariant: Pull computations are always idempotent (`forall x :: Pull a. x *> x = x`).
newtype Pull a = MkPull (ReaderT Time IOSync a)

runPull :: forall a. Time -> Pull a -> IOSync a
runPull time (MkPull x) = runReaderT x time

derive newtype instance functorPull :: Functor Pull
derive newtype instance applyPull :: Apply Pull
derive newtype instance applicativePull :: Applicative Pull
derive newtype instance bindPull :: Bind Pull
derive newtype instance monadPull :: Monad Pull

getTime :: Pull Time
getTime =
  -- ask is idempotent
  MkPull ask

pullReadIORef :: forall a. IORef a -> Pull a
pullReadIORef ref =
  -- readIORef is idempotent
  MkPull $ lift $ readIORef ref

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
  ref <- newIORef Fresh
  pure $ MkPull $ ReaderT $ \time -> do
    cache <- readIORef ref
    case cache of
      Cached lastTime value | lastTime == time ->
        pure value

      BlackHole ->
        unsafeCrashWith "Illegal self-referential computation passed to oncePerFrame"

      _ -> do
        writeIORef ref BlackHole
        value <- runPull time action >>= io
        writeIORef ref (Cached time value)
        pure value

class Monad m <= MonadPull m where
  pull :: forall a. Pull a -> m a

instance monadPullIOSync :: MonadPull IOSync where
  pull p = do
    time <- freshTime
    runPull time p

instance monadPullCleanupT :: MonadPull m => MonadPull (CleanupT m) where
  pull = lift <<< pull

instance monadPullReaderT :: MonadPull m => MonadPull (ReaderT r m) where
  pull = lift <<< pull

-------------------------------------------------

-- | Computations that occur during a Frame.
--
-- During a frame, no arbitrary effects are performed. Instead they are
-- registered using `effect` to be performed after the frame.
--
-- Frame computations have access to current logical time. See `oncePerFrame`
-- for why this is needed.
newtype Frame a = Frame (ReaderT DelayedEffects Pull a)

framePull :: forall a. Pull a -> Frame a
framePull = Frame <<< lift

frameWriteIORef :: forall a. IORef a -> a -> Frame Unit
frameWriteIORef ref value =
  -- HACK: briefly creating a Pull that is not idempotent;
  -- But it's immediately lifted to Frame, so it's OK
  framePull $ MkPull $ lift $ writeIORef ref value

-- | Schedule an effect to be executed after the Frame completed.
effect :: IOSync Unit -> Frame Unit
effect action = Frame $ ReaderT $ \effects ->
  -- HACK: briefly creating a Pull that is not idempotent;
  -- But it's immediately lifted to Frame, so it's OK
  void $ MkPull $ lift $ DE.push effects action

derive newtype instance functorFrame :: Functor Frame
derive newtype instance applyFrame :: Apply Frame
derive newtype instance applicativeFrame :: Applicative Frame
derive newtype instance bindFrame :: Bind Frame
derive newtype instance monadFrame :: Monad Frame

-- | Run a Frame computation and then run its effects.
runFrame :: forall a. Time -> Frame a -> IOSync a
runFrame currentTime (Frame x) = do
  effectsMutable <- DE.empty
  value <- runPull currentTime $ runReaderT x effectsMutable
  effects <- DE.unsafeFreeze effectsMutable
  DE.sequenceEffects effects
  pure value

freshTime :: IOSync Time
freshTime = do
  time <- readIORef nextTimeRef
  writeIORef nextTimeRef (case time of Time t -> Time (t + 1))
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
  ref <- newIORef Nothing
  pure $ do
    time <- framePull $ getTime
    m_lastTime <- framePull $ pullReadIORef ref
    case m_lastTime of
      Just lastTime | lastTime == time ->
        pure unit
      _ -> do
        frameWriteIORef ref (Just time)
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
newtype Behavior a = Behavior (Pull a)
-- Behavior is represented by a computation that reads its value.

-- | Read a value of a Behavior.
readBehavior :: forall a. Behavior a -> Pull a
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

instance functorEvent :: Functor Event where
  map f (Event {occurence, subscribe}) = Event { occurence: map (map f) occurence, subscribe }

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

class Monad m <= MonadHostCreate io m | m -> io where
  -- | Create an Event that can be triggered externally.
  -- | Each `fire` will run a frame where the event occurs.
  newEvent :: forall a. m { event :: Event a, fire :: a -> io Unit }

  -- | Create a new Behavior whose value can be modified outside a frame.
  newBehavior :: forall a. a -> m { behavior :: Behavior a, set :: a -> io Unit }

instance monadHostCreateReaderT :: MonadHostCreate io m => MonadHostCreate io (ReaderT r m) where
  newEvent = lift newEvent
  newBehavior = lift <<< newBehavior

class (Monad io, Monad m, MonadPull m, MonadCleanup m, MonadHostCreate io m) <= MonadHost io m | m -> io where
  subscribeEvent_ :: forall a. (a -> io Unit) -> Event a -> m Unit

  hostEffect :: forall a. io a -> m a

instance monadHostReaderT :: (Monad io, MonadHost io m) => MonadHost io (ReaderT r m) where
  subscribeEvent_ f event = lift (subscribeEvent_ f event)
  hostEffect = lift <<< hostEffect

instance monadHostCreateIOSync :: MonadHostCreate IOSync IOSync where
  newEvent = do
    occurenceRef <- newIORef Nothing
    listenerMap <- UMM.new
    let
      fire value = do
        writeIORef occurenceRef (Just value)
        listeners <- UMM.values listenerMap
        runNextFrame $ do
          sequenceFrame_ listeners
          frameWriteIORef occurenceRef Nothing

      subscribe l = do
        key <- UMM.insert l listenerMap
        pure $ UMM.delete key listenerMap

    pure
      { event: Event { occurence: Behavior $ pullReadIORef occurenceRef, subscribe }
      , fire
      }

  newBehavior initialValue = newBehaviorIOSync initialValue

foreign import sequenceFrame_ :: Array (Frame Unit) -> Frame Unit

instance monadHostCreateCleanupT :: (Monad m, MonadHostCreate io m) => MonadHostCreate io (CleanupT m) where
  newEvent = lift newEvent
  newBehavior = lift <<< newBehavior

instance monadHostCleanupT :: MonadHost IOSync (CleanupT IOSync) where
  subscribeEvent_ handler (Event {occurence,subscribe}) = do
    unsub <- liftIOSync $ subscribe $ do
      m_value <- framePull $ readBehavior occurence
      for_ m_value $ \value ->
        effect $ handler value
    onCleanup unsub

  hostEffect = liftIOSync

newBehaviorIOSync :: forall a. a -> IOSync { behavior :: Behavior a, set :: a -> IOSync Unit }
newBehaviorIOSync initialValue = do
  ref <- newIORef initialValue
  pure
    { behavior: Behavior $ pullReadIORef ref
    , set: writeIORef ref
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

newtype Dynamic a = Dynamic
  { value :: Behavior a
  , change :: Event Unit
  }

-- | The Behavior representing the current value of the Dynamic.
-- | When it is changing (the change event occurs), it has the new value.
current :: forall a. Dynamic a -> Behavior a
current (Dynamic {value}) = value

-- | An Event that fires with the new value every time the Dynamic changes.
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

class MonadCleanup m <= MonadHold m where
  -- | `foldDyn f x e` - Make a Dynamic that will have the initial value `x`,
  -- | and every time `e` fires, its value will update by applying `f` to the
  -- | event occurence value and the old value.
  -- |
  -- | On cleanup, the Dynamic will stop updating in response to the event.
  foldDyn :: forall a b. (a -> b -> b) -> b -> Event a -> m (Dynamic b)

  -- | Like `foldDyn`, but the Dynamic will not update if the folding function
  -- | returns Nothing.
  foldDynMaybe :: forall a b. (a -> b -> Maybe b) -> b -> Event a -> m (Dynamic b)

instance monadHoldReaderT :: MonadHold m => MonadHold (ReaderT r m) where
  foldDyn f x0 e = lift (foldDyn f x0 e)
  foldDynMaybe f x0 e = lift (foldDynMaybe f x0 e)

instance monadHoldCleanupT :: MonadHold (CleanupT IOSync) where
  foldDyn f initial (Event event) = do
    ref <- liftIOSync $ newIORef initial
    updateOrReadValue <- liftIOSync $
      oncePerFramePullWithIO (readBehavior event.occurence) $ \m_newValue -> do
        oldValue <- readIORef ref
        case m_newValue of
          Just occurence -> do
            let newValue = f occurence oldValue
            writeIORef ref newValue
            pure newValue
          Nothing ->
            pure oldValue

    unsub <- liftIOSync $ event.subscribe $ void $ framePull $ updateOrReadValue
    onCleanup unsub

    pure $ Dynamic
      { value: Behavior updateOrReadValue
      , change: map (\_ -> unit) (Event event)
      }

  foldDynMaybe = foldDynMaybeImpl

foldDynMaybeImpl :: forall a b. (a -> b -> Maybe b) -> b -> Event a -> CleanupT IOSync (Dynamic b)
foldDynMaybeImpl f initial (Event event) = do
  ref <- liftIOSync $ newIORef initial
  (updateOrReadValue :: Pull { changing :: Boolean, value :: b }) <- liftIOSync $
    oncePerFramePullWithIO (readBehavior event.occurence) $ \m_newValue -> do
      oldValue <- readIORef ref
      case m_newValue of
        Just occurence | Just newValue <- f occurence oldValue -> do
          writeIORef ref newValue
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

holdDyn :: forall m a. MonadHold m => a -> Event a -> m (Dynamic a)
holdDyn = foldDyn (\x _ -> x)

holdUniqDynBy :: forall m a. MonadHold m => (a -> a -> Boolean) -> a -> Event a -> m (Dynamic a)
holdUniqDynBy eq = foldDynMaybe (\new old -> if eq new old then Nothing else Just new)

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
        updateListener = framePull (readBehavior value) >>= replaceWith

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
     forall io m a
   . MonadHost io m
  => Monad m -- FIXME: why is this needed?
  => (a -> io Unit)
  -> Dynamic a
  -> m Unit
subscribeDyn_ handler (Dynamic {value, change}) = do
  currentValue <- pull $ readBehavior value
  hostEffect $ handler currentValue
  subscribeEvent_ handler (mapEventB (\_ -> value) change)

subscribeDyn ::
     forall io m a b
   . MonadHost io m
  => MonadHold m
  => Monad io -- FIXME: why is this needed?
  => (a -> io b)
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

latestJust :: forall m a. MonadPull m => MonadHold m => Dynamic (Maybe a) -> m (Dynamic (Maybe a))
latestJust dyn = do
  currentValue <- pull $ readBehavior $ current dyn
  foldDynMaybe (\new _ -> map Just new) currentValue (changed dyn)

class (MonadHold m, MonadHost IOSync m, MonadIOSync m) <= MonadFRP m
instance monadFRP :: (MonadHold m, MonadHost IOSync m, MonadIOSync m) => MonadFRP m

for :: forall f a b. Functor f => f a -> (a -> b) -> f b
for = flip map
