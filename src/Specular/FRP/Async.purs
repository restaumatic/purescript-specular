module Specular.FRP.Async
  ( startAff

  , RequestState(..)
  , fromLoaded

  , asyncRequestMaybe
  , asyncRequest

  , performEvent
  ) where

import Prelude

import Control.Monad.Cleanup (onCleanup)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(Just, Nothing))
import Effect.Aff (Aff, error, killFiber, launchAff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Specular.FRP (class MonadFRP, Dynamic, Event, changed, current, holdDyn, leftmost, newEvent, pull, subscribeDyn_)
import Specular.FRP.Base (readBehavior, subscribeEvent_)
import Effect.Ref (new, write, read)

-- | Start an asynchronous Aff computation. It will be cancelled on cleanup.
startAff :: forall m. MonadFRP m => Aff Unit -> m Unit
startAff action = do
  fiber <- liftEffect $ launchAff action
  onCleanup $ launchAff_ $ killFiber (error "Cancelled") fiber

data RequestState a = NotRequested | Loading | Loaded a

derive instance eqRequestState :: Eq a => Eq (RequestState a)
derive instance genericRequestState :: Generic (RequestState a) _
derive instance functorRequestState :: Functor RequestState

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
-- |   using Aff's (Aff's) cancellation mechanism. This ensures that the value of the resulting Dynamic
-- |   will eventually be that of the most recent value of the input Dynamic,
-- |   independent of the order of arrival of the responses.
asyncRequestMaybe :: forall m a
   . MonadEffect m
  => MonadFRP m
  => Dynamic (Maybe (Aff a))
  -> m (Dynamic (RequestState a))
asyncRequestMaybe dquery = do
  loadStateChanged <- newEvent
  cancelCurrentRef <- liftEffect $ new (pure unit)

  let
    update m_query = do
      cancelCurrent <- read cancelCurrentRef
      cancelCurrent

      case m_query of
        Nothing ->
          write (pure unit) cancelCurrentRef
        Just query -> do
          fiber <- launchAff $ runAndUpdateResult query
          write (launchAff_ $ killFiber (error "Cancelled") fiber) cancelCurrentRef

    runAndUpdateResult query = do
      value <- query
      liftEffect $ loadStateChanged.fire (Loaded value)

  let
    -- Status when the request starts.
    initialStatus (Just _) = Loading
    initialStatus Nothing  = NotRequested

  initialValue <- pull $ readBehavior $ current dquery
  dyn <- holdDyn (initialStatus initialValue) $
    leftmost
      [ initialStatus <$> changed dquery
      , loadStateChanged.event
      ]

  -- Note: we have to subscribe after we start listening to `loadStateChanged`,
  -- else we could miss the result of an initial synchronous action
  subscribeDyn_ update dquery

  pure dyn

-- | Like `asyncRequestMaybe`, but without the Nothing case.
asyncRequest :: forall m a
   . MonadEffect m
  => MonadFRP m
  => Dynamic (Aff a)
  -> m (Dynamic (RequestState a))
asyncRequest dquery = asyncRequestMaybe (map Just dquery)

-- | Run asynchronous action when an Event occurs.
-- | The returned Event will fire when such an action completes.
-- |
-- | Note: the results may arrive in a different order than the requests.
-- FIXME: Does not cancel running actions on cleanup
performEvent
  :: forall m a
   . MonadFRP m
  => Event (Aff a)
  -> m (Event a)
performEvent event = do
  output <- newEvent
  subscribeEvent_ (\action ->
    void $ launchAff do
      x <- action
      liftEffect $ output.fire x
    ) event
  pure output.event
