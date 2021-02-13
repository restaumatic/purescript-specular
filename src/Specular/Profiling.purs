module Specular.Profiling
  ( module Specular.Profiling
  , module X
  ) where

import Prelude

import Effect
import Effect.Class
import Effect.Uncurried
import Specular.Internal.Profiling as Internal
import Specular.Internal.Profiling (enabled, Mark) as X

begin :: String -> Effect Internal.Mark
begin = if Internal.enabled then runEffectFn1 Internal.begin else \_ -> pure Internal.none

end :: Internal.Mark -> Effect Unit
end = if Internal.enabled then runEffectFn1 Internal.end else \_ -> pure unit

measure :: forall m a. MonadEffect m => String -> m a -> m a
measure =
  if Internal.enabled then \name action -> do
    mark <- liftEffect $ runEffectFn1 Internal.begin name
    result <- action
    liftEffect $ runEffectFn1 Internal.end mark
    pure result
  else
    \_ action -> action

