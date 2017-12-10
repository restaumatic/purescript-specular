module Specular.FRP.Replaceable where

import Prelude

import Control.Monad.IOSync (IOSync)
import Control.Monad.Replace (class MonadReplace, runReplaceable)
import Specular.FRP.Base (class MonadHold, class MonadHost, Dynamic, subscribeDyn, subscribeDyn_)
import Specular.FRP.WeakDynamic (WeakDynamic, subscribeWeakDyn, subscribeWeakDyn_)

dynamic_ :: forall m. MonadReplace m => MonadHost IOSync m => Dynamic (m Unit) -> m Unit
dynamic_ dyn = do
  {replace} <- runReplaceable
  subscribeDyn_ replace dyn

dynamic :: forall m a. MonadReplace m => MonadHold m => MonadHost IOSync m => Dynamic (m a) -> m (Dynamic a)
dynamic dyn = do
  {replace} <- runReplaceable
  subscribeDyn replace dyn

weakDynamic_ :: forall m. MonadReplace m => MonadHost IOSync m => WeakDynamic (m Unit) -> m Unit
weakDynamic_ dyn = do
  {replace} <- runReplaceable
  subscribeWeakDyn_ replace dyn

weakDynamic :: forall m a. MonadReplace m => MonadHold m => MonadHost IOSync m => WeakDynamic (m a) -> m (WeakDynamic a)
weakDynamic dyn = do
  {replace} <- runReplaceable
  subscribeWeakDyn replace dyn
