module Specular.FRP.JS
  ( newEvent
  , subscribeEvent_

  , newDynamic
  , holdDyn
  , changed

  , bindDyn
  , mapDyn
  ) where

import Prelude

import Control.Monad.Cleanup (runCleanupT)
import Control.Monad.IOSync (IOSync)
import Data.Tuple (fst)
import Specular.FRP (Dynamic, Event)
import Specular.FRP as FRP

newEvent :: forall a. IOSync { event :: Event a, fire :: a -> IOSync Unit }
newEvent = FRP.newEvent

newDynamic :: forall a. a -> IOSync { dyn :: Dynamic a, fire :: a -> IOSync Unit }
newDynamic initial = do
  { event, fire } <- FRP.newEvent
  dyn <- holdDyn initial event
  pure { dyn, fire }

holdDyn :: forall a. a -> Event a -> IOSync (Dynamic a)
holdDyn initial event = map fst $ runCleanupT $ FRP.holdDyn initial event

changed :: forall a. Dynamic a -> Event a
changed = FRP.changed

subscribeEvent_ :: forall a. (a -> IOSync Unit) -> Event a -> IOSync Unit
subscribeEvent_ handle event = map fst $ runCleanupT $ FRP.subscribeEvent_ handle event

mapDyn :: forall a b. (a -> b) -> Dynamic a -> Dynamic b
mapDyn = map

bindDyn :: forall a b. Dynamic a -> (a -> Dynamic b) -> Dynamic b
bindDyn = bind
