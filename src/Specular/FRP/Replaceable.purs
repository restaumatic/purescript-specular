module Specular.FRP.Replaceable where

import Prelude

import Control.Monad.Replace (class MonadReplace, newSlot, replaceSlot)
import Data.Maybe (Maybe(..), isJust)
import Specular.FRP.Base (class MonadFRP, Dynamic, changed, filterJustEvent, newDynamic, readDynamic, subscribeDyn, subscribeDyn_, subscribeEvent_, uniqDyn)

dynamic_ :: forall m. MonadReplace m => MonadFRP m => Dynamic (m Unit) -> m Unit
dynamic_ dyn = do
  slot <- newSlot
  subscribeDyn_ (replaceSlot slot) dyn

withDynamic_ :: forall m a. MonadReplace m => MonadFRP m => Dynamic a -> (a -> m Unit) -> m Unit
withDynamic_ dyn widget = do
  slot <- newSlot
  subscribeDyn_ (\x -> replaceSlot slot (widget x)) dyn

dynamic :: forall m a. MonadReplace m => MonadFRP m => Dynamic (m a) -> m (Dynamic a)
dynamic dyn = do
  slot <- newSlot
  subscribeDyn (replaceSlot slot) dyn

whenJustD :: forall m a. MonadReplace m => MonadFRP m => Dynamic (Maybe a) -> (Dynamic a -> m Unit) -> m Unit
whenJustD dyn widget = do
  whenD (isJust <$> dyn) do
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
