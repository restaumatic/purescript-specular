module Debug.Specular
  ( traceEventWith
  , traceEvent
  , traceDynWith
  , traceDyn
  ) where

import Prelude

import Debug.Trace (class DebugWarning, trace)
import Specular.FRP (Dynamic, Event)

-- | Note: tracing will work only when something will subscribe
-- | to returned Event.
traceEventWith :: forall a. DebugWarning => (a -> String) -> Event a -> Event a
traceEventWith f = map (\a -> trace (f a) (const a))

traceEvent :: forall a. DebugWarning => Show a => String -> Event a -> Event a
traceEvent prefix = traceEventWith (\a -> prefix <> ": " <> show a)

-- | Note: tracing will work only when something will subscribe
-- | to returned Dynamic.
traceDynWith :: forall a. DebugWarning => (a -> String) -> Dynamic a -> Dynamic a
traceDynWith f = map (\a -> trace (f a) (const a))

traceDyn :: forall a. DebugWarning => Show a => String -> Dynamic a -> Dynamic a
traceDyn prefix = traceDynWith (\a -> prefix <> ": " <> show a)
