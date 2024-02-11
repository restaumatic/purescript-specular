module Specular.FRP
  ( module X.Base
  , module X.Replaceable
  ) where

import Specular.FRP.Base (class MonadFRP, Behavior, Dynamic, Event, _subscribeEvent, annotate, annotated, attachDynWith, changed, changed_, current, filterEvent, filterJustEvent, filterMapEvent, foldDyn, foldDynMaybe, holdDyn, holdUniqDynBy, latestJust, leftmost, map2, never, newBehavior, newDynamic, newEvent, pull, readBehavior, readDynamic, sampleAt, subscribeDyn, subscribeDyn_, subscribeEvent_, switch, tagDyn, traceDynIO, traceEventIO, uniqDyn, uniqDynBy) as X.Base
import Specular.FRP.Replaceable (dynamic, dynamic_, unlessD, whenD, whenJustD, withDynamic_) as X.Replaceable
