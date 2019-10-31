module Specular.FRP.Replaceable where

import Prelude

import Control.Monad.Replace (class MonadReplace, Slot(Slot), newSlot)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Partial.Unsafe (unsafeCrashWith)
import Specular.FRP.Base (class MonadFRP, Dynamic, changed, filterJustEvent, newDynamic, readDynamic, subscribeDyn, subscribeDyn_, subscribeEvent_, uniqDyn, uniqDynBy)
import Specular.FRP.WeakDynamic (WeakDynamic, subscribeWeakDyn, subscribeWeakDyn_)

dynamic_ :: forall m. MonadReplace m => MonadFRP m => Dynamic (m Unit) -> m Unit
dynamic_ dyn = do
  Slot {replace} <- newSlot
  subscribeDyn_ (\x -> replace x) dyn

withDynamic_ :: forall m a. MonadReplace m => MonadFRP m => Dynamic a -> (a -> m Unit) -> m Unit
withDynamic_ dyn widget = do
  Slot {replace} <- newSlot
  subscribeDyn_ (\x -> replace (widget x)) dyn

dynamic :: forall m a. MonadReplace m => MonadFRP m => Dynamic (m a) -> m (Dynamic a)
dynamic dyn = do
  Slot {replace} <- newSlot
  subscribeDyn (\x -> replace x) dyn

weakDynamic_ :: forall m. MonadReplace m => MonadFRP m => WeakDynamic (m Unit) -> m Unit
weakDynamic_ dyn = do
  Slot {replace} <- newSlot
  subscribeWeakDyn_ (\x -> replace x) dyn

weakDynamic :: forall m a. MonadReplace m => MonadFRP m => WeakDynamic (m a) -> m (WeakDynamic a)
weakDynamic dyn = do
  Slot {replace} <- newSlot
  subscribeWeakDyn (\x -> replace x) dyn

whenJustD :: forall m a. MonadReplace m => MonadFRP m => Dynamic (Maybe a) -> (Dynamic a -> m Unit) -> m Unit
whenJustD dyn widget = do
  shouldDisplay <- uniqDynBy eq (isJust <$> dyn)
  whenD shouldDisplay do
    dynVal <- readDynamic dyn
    case dynVal of
      Just dynVal' -> do
        dyn' <- newDynamic dynVal'
        subscribeEvent_ dyn'.set $ filterJustEvent $ changed dyn
        widget dyn'.dynamic
      Nothing -> pure unit

-- | Execute a monadic action in a Replaceable monad only if the given Dynamic
-- | has value `true`.
whenD :: forall m. MonadFRP m => MonadReplace m => Dynamic Boolean -> m Unit -> m Unit
whenD dyn block = do
  dyn' <- uniqDyn dyn
  dynamic_ $ dyn' <#> \value -> when value block

-- | Execute a monadic action in a Replaceable monad only if the given Dynamic
-- | has value `true`.
unlessD :: forall m. MonadFRP m => MonadReplace m => Dynamic Boolean -> m Unit -> m Unit
unlessD dyn block = do
  dyn' <- uniqDyn dyn
  dynamic_ $ dyn' <#> \value -> unless value block
