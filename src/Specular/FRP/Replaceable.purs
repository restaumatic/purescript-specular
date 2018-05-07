module Specular.FRP.Replaceable where

import Prelude

import Control.Monad.Replace (class MonadReplace, Slot(Slot), newSlot)
import Specular.FRP.Base (class MonadFRP, Dynamic, subscribeDyn, subscribeDyn_)
import Specular.FRP.WeakDynamic (WeakDynamic, subscribeWeakDyn, subscribeWeakDyn_)

dynamic_ :: forall m. MonadReplace m => MonadFRP m => Dynamic (m Unit) -> m Unit
dynamic_ dyn = do
  Slot {replace} <- newSlot
  subscribeDyn_ (\x -> replace x) dyn

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
