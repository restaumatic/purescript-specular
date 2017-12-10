module Specular.FRP.Replaceable where

import Prelude

import Control.Monad.IOSync (IOSync)
import Control.Monad.Replace (class MonadReplace, runReplaceable)
import Specular.FRP.Base (class MonadHost, Dynamic, subscribeDyn_)
import Specular.FRP.WeakDynamic (WeakDynamic, subscribeWeakDyn_)

dynamic_ :: forall m. MonadReplace m => MonadHost IOSync m => Dynamic (m Unit) -> m Unit
dynamic_ dyn = do
  {replace} <- runReplaceable
  subscribeDyn_ replace dyn

weakDynamic_ :: forall m. MonadReplace m => MonadHost IOSync m => WeakDynamic (m Unit) -> m Unit
weakDynamic_ dyn = do
  {replace} <- runReplaceable
  subscribeWeakDyn_ replace dyn
