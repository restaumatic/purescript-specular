module Specular.FRP.List where

import Prelude

import Control.Monad.IOSync (IOSync(..))
import Control.Monad.IOSync.Class (liftIOSync)
import Control.Monad.Replace (class MonadReplace, Slot(..), newSlot, unSlot)
import Data.Array as Array
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Specular.FRP.Base (class MonadFRP, holdDyn, hostEffect, newEvent)
import Specular.FRP.WeakDynamic (WeakDynamic, holdWeakDyn, subscribeWeakDyn, subscribeWeakDyn_, weaken)

weakDynamicList_ :: forall m a
   . MonadFRP m
  => MonadReplace m
  => WeakDynamic (Array a)
  -> (WeakDynamic a -> m Unit)
  -> m Unit
weakDynamicList_ dynArray handler = do
  latestRef :: IORef (Array { slot :: Slot m, fire :: a -> IOSync Unit }) <- hostEffect $ newIORef []
  mainSlot <- newSlot
  pure unit
  
  let
    update :: Array a -> IOSync Unit
    update newArray = do
      latest <- readIORef latestRef
      newEntries <- map Array.concat $ flip traverse (Array.range 0 (max (Array.length newArray) (Array.length latest))) $ \i -> do
        case Array.index latest i, Array.index newArray i of
          Just entry, Just x  -> do
            entry.fire x
            pure []
          Just entry, Nothing -> do
            (unSlot entry.slot).destroy
            pure []
          Nothing,     Just x  -> do
            slot <- (unSlot mainSlot).append
            {event, fire} <- newEvent
            (unSlot slot).replace $ do
              wdyn <- weaken <$> holdDyn x event
              handler wdyn
            pure [{slot, fire}]
          Nothing,     Nothing ->
            pure []
      writeIORef latestRef (Array.take (Array.length newArray) $ latest <> newEntries)

  subscribeWeakDyn_ update dynArray
