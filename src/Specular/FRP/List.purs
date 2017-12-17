module Specular.FRP.List where

import Prelude

import Control.Monad.IOSync (IOSync)
import Control.Monad.Replace (class MonadReplace, Slot, newSlot, unSlot)
import Data.Array as Array
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Specular.FRP.Base (class MonadFRP, holdUniqDynBy, hostEffect, newEvent)
import Specular.FRP.WeakDynamic (WeakDynamic, holdWeakDyn, subscribeWeakDyn_, weaken)
import Unsafe.Reference (unsafeRefEq)

-- | `weakDynamicList dynArray handler`
-- | Render a list of items from `dynArray`. Each item will be rendered by `handler`.
-- |
-- | When the array changes, indexes that exist in both old and new array are _updated_,
-- | e.g. the Dynamics passed to handlers are changed. As an optimization,
-- | when old and new item values are the same JS object (using `===`), the
-- | Dynamic is not updated.
-- |
-- | If the array grows (a new index appears), a new handler is invoked.
-- |
-- | The resulting WeakDynamic represents return values from all the handlers.
weakDynamicList :: forall m a b
   . MonadFRP m
  => MonadReplace m
  => WeakDynamic (Array a)
  -> (WeakDynamic a -> m b)
  -> m (WeakDynamic (Array b))
weakDynamicList dynArray handler = do
  (latestRef :: IORef (Array { slot :: Slot m
                             , fire :: a -> IOSync Unit
                             , result :: b }))
    <- hostEffect $ newIORef []

  mainSlot <- newSlot
  resultChanged <- newEvent
  
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
            result <- (unSlot slot).replace $ do
              wdyn <- weaken <$> holdUniqDynBy unsafeRefEq x event
              handler wdyn
            pure [{slot, fire, result}]
          Nothing,     Nothing ->
            pure []
      let newLatest = Array.take (Array.length newArray) $ latest <> newEntries
      writeIORef latestRef newLatest
      resultChanged.fire $ map _.result newLatest

  subscribeWeakDyn_ update dynArray

  holdWeakDyn resultChanged.event

weakDynamicList_ :: forall m a
   . MonadFRP m
  => MonadReplace m
  => WeakDynamic (Array a)
  -> (WeakDynamic a -> m Unit)
  -> m Unit
weakDynamicList_ dynArray handler = void $ weakDynamicList dynArray handler
