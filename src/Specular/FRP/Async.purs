module Specular.FRP.Async
  ( startIO

  , RequestState(..)
  , fromLoaded

  , asyncRequestMaybe
  , asyncRequest

  , performEvent
  ) where

import Prelude

import Control.Monad.Aff (killFiber, launchAff, launchAff_)
import Control.Monad.Cleanup (class MonadCleanup, onCleanup)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.IO (IO, runIO)
import Control.Monad.IOSync (IOSync)
import Control.Monad.IOSync.Class (class MonadIOSync, liftIOSync)
import Control.Monad.Replace (class MonadReplace)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(Just, Nothing))
import Specular.FRP (class MonadFRP, class MonadHost, Dynamic, Event, changed, current, dynamic_, holdDyn, leftmost, newEvent, pull)
import Specular.FRP.Base (readBehavior, subscribeEvent_)

-- | Start an asynchronous IO computation. It will be cancelled on cleanup.
startIO :: forall m. MonadIOSync m => MonadCleanup m => IO Unit -> m Unit
startIO action = do
  fiber <- liftIOSync $ liftEff $ launchAff $ runIO action
  onCleanup $ liftEff $ launchAff_ $ killFiber (error "Cancelled") fiber

data RequestState a = NotRequested | Loading | Loaded a

derive instance eqRequestState :: Eq a => Eq (RequestState a)
derive instance genericRequestState :: Generic (RequestState a) _

instance showRequestState :: Show a => Show (RequestState a) where
  show = genericShow

fromLoaded :: forall a. RequestState a -> Maybe a
fromLoaded (Loaded x) = Just x
fromLoaded _ = Nothing

-- | On each observed value of the Dynamic,
-- | - if it's Nothing, the resulting Dynamic will have value NotRequested.
-- | - if it's a Just, the contents will be executes asynchronously and the
-- |   resulting Dynamic will obtain value Loaded x. During the request execution
-- |   the result Dynamic will have value Loading.
-- |
-- |   If the input Dynamic changes value while a request is in progress, it will be cancelled
-- |   using IO's (Aff's) cancellation mechanism. This ensures that the value of the resulting Dynamic
-- |   will eventually be that of the most recent value of the input Dynamic,
-- |   independent of the order of arrival of the responses.
-- |
-- | FIXME: The resulting Dynamic does not change to Loading in the same frame as the input Dynamic changes value.
-- |
-- | FIXME: What is the behavior of IO with nonCanceler? Will the result be
-- | suppressed? If not, we need additional mechanism here to prevent
-- | out-of-order responses.
asyncRequestMaybe :: forall m a
   . MonadIOSync m
  => MonadFRP m
  => MonadReplace m
  => Dynamic (Maybe (IO a))
  -> m (Dynamic (RequestState a))
asyncRequestMaybe dquery = do
  loadStateChanged <- newEvent

  -- NB: It's important that `holdDyn` is before `dynamic_`.
  -- If the initial value is a Query, the resulting Dynamic should
  -- have value Loading, not NotRequested.
  dyn <- holdDyn NotRequested loadStateChanged.event

  dynamic_ $ flip map dquery $ \mquery ->
    case mquery of
      Nothing ->
        liftIOSync $ loadStateChanged.fire NotRequested
      Just query ->
        startIO $ do
          liftIOSync $ loadStateChanged.fire Loading
          value <- query
          liftIOSync $ loadStateChanged.fire (Loaded value)

  pure dyn

-- | Like `asyncRequestMaybe`, but without the Nothing case.
asyncRequest :: forall m a
   . MonadIOSync m
  => MonadFRP m
  => MonadReplace m
  => Dynamic (IO a)
  -> m (Dynamic (RequestState a))
asyncRequest dquery = asyncRequestMaybe (map Just dquery)

-- | Run asynchronous action when an Event occurs.
-- | The returned Event will fire when such an action completes.
-- |
-- | Note: the results may arrive in a different order than the requests.
-- FIXME: Does not cancel running actions on cleanup
performEvent
  :: forall m a
   . MonadHost IOSync m
  => Event (IO a)
  -> m (Event a)
performEvent event = do
  output <- newEvent
  subscribeEvent_ (\action ->
    void $ liftEff $ launchAff $ runIO $ do
      x <- action
      liftIOSync $ output.fire x
    ) event
  pure output.event
