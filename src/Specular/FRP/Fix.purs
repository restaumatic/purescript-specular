module Specular.FRP.Fix (
    fixDyn
  , fixEvent
) where

import Prelude

import Control.Monad.Cleanup (class MonadCleanup)
import Control.Monad.IOSync.Class (class MonadIOSync, liftIOSync)
import Data.Tuple (Tuple(..))
import Specular.FRP.Base (Dynamic, Event, newEvent, subscribeDyn_, subscribeEvent_)
import Specular.FRP.WeakDynamic (WeakDynamic, holdWeakDyn)

fixEvent ::
     forall m a b
   . MonadCleanup m
  => MonadIOSync m
  => (Event a -> m (Tuple (Event a) b))
  -> m b
fixEvent f = do
  {event,fire} <- liftIOSync newEvent
  Tuple event' result <- f event
  subscribeEvent_ fire event'
  pure result

fixDyn ::
     forall m a b
   . MonadCleanup m
  => MonadIOSync m
  => (WeakDynamic a -> m (Tuple (Dynamic a) b))
  -> m b
fixDyn f = do
  {event,fire} <- liftIOSync newEvent
  wdyn <- holdWeakDyn event
  Tuple dyn result <- f wdyn
  subscribeDyn_ fire dyn
  pure result
