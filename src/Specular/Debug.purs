module Specular.Debug
  ( traceEventWith
  , traceEvent
  , traceDynWith
  , traceDyn
  ) where

import Prelude

import Debug.Trace (class DebugWarning, trace)
import Specular.FRP (Dynamic, Event)
import Specular.FRP.Base (traceDynIO, traceEventIO)

-- | Note: tracing will work only when something will subscribe
-- | to returned Event.
traceEventWith :: forall a. DebugWarning => (a -> String) -> Event a -> Event a
traceEventWith f = traceEventIO (\a -> trace (f a) (\_ -> pure unit))

traceEvent :: forall a. DebugWarning => Show a => String -> Event a -> Event a
traceEvent prefix = traceEventWith (\a -> prefix <> ": " <> show a)

-- | Note: tracing will work only when something will subscribe
-- | to returned Dynamic.
traceDynWith :: forall a. DebugWarning => (a -> String) -> Dynamic a -> Dynamic a
traceDynWith f = traceDynIO (\a -> trace (f a) (\_ -> pure unit))

traceDyn :: forall a. DebugWarning => Show a => String -> Dynamic a -> Dynamic a
traceDyn prefix = traceDynWith (\a -> prefix <> ": " <> show a)
