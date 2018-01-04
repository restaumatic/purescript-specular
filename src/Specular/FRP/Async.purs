module Specular.FRP.Async
  ( startIO

  , RequestState(..)
  , fromLoaded

  , asyncRequestMaybe
  , asyncRequest
  ) where

import Prelude

import Control.Monad.Aff (killFiber, launchAff, launchAff_)
import Control.Monad.Cleanup (class MonadCleanup, onCleanup)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.IO (IO, runIO)
import Control.Monad.IOSync.Class (class MonadIOSync, liftIOSync)
import Control.Monad.Replace (class MonadReplace)
import Data.Maybe (Maybe(..))
import Specular.FRP (class MonadFRP, Dynamic, dynamic_, holdDyn, newEvent)

-- | Start an asynchronous IO computation. It will be cancelled on cleanup.
startIO :: forall m. MonadIOSync m => MonadCleanup m => IO Unit -> m Unit
startIO action = do
  fiber <- liftIOSync $ liftEff $ launchAff $ runIO action
  onCleanup $ liftEff $ launchAff_ $ killFiber (error "Cancelled") fiber

data RequestState a = NotRequested | Loading | Loaded a

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
