module Specular.Dom.Builder.Class where

import Prelude

import Control.Monad.Cleanup (onCleanup)
import Control.Monad.IOSync (IOSync)
import Control.Monad.IOSync.Class (class MonadIOSync)
import Control.Monad.Replace (class MonadReplace)
import Data.Monoid (mempty)
import Data.Tuple (Tuple, snd)
import Specular.Dom.Browser (Node)
import Specular.Dom.Node.Class (class EventDOM, Attrs, EventType, addEventListener)
import Specular.FRP (class MonadHold, class MonadHost, Event, WeakDynamic, hostEffect, newEvent, weakDynamic_)

class Monad m <= MonadDomBuilder node m | m -> node where
  text :: String -> m Unit
  elDynAttr' :: forall a . String -> WeakDynamic Attrs -> m a -> m (Tuple node a)

elDynAttr ::
     forall node m a
   . MonadDomBuilder node m
  => String
  -> WeakDynamic Attrs
  -> m a
  -> m a
elDynAttr tagName dynAttrs inner = snd <$> elDynAttr' tagName dynAttrs inner

elAttr ::
     forall node m a
   . MonadDomBuilder node m
  => String
  -> Attrs
  -> m a
  -> m a
elAttr tagName attrs inner = elDynAttr tagName (pure attrs) inner

el ::
     forall node m a
   . MonadDomBuilder node m
  => String
  -> m a
  -> m a
el tagName inner = elAttr tagName mempty inner

dynText :: forall node m
  . MonadDomBuilder node m
 => MonadHost IOSync m
 => MonadReplace m
 => WeakDynamic String
 -> m Unit
dynText = weakDynamic_ <<< map text

domEventWithSample ::
     forall event node m a
   . EventDOM event node
  => MonadHost IOSync m
  => (event -> IOSync a)
  -> EventType
  -> node
  -> m (Event a)
domEventWithSample sample eventType node = do
  {event,fire} <- newEvent
  unsub <- hostEffect $ addEventListener eventType (sample >=> fire) node
  onCleanup unsub
  pure event

-- A handy alias
class (MonadDomBuilder Node m, MonadHost IOSync m, MonadReplace m, MonadHold m, MonadIOSync m) <= MonadWidget m
instance monadWidget :: (MonadDomBuilder Node m, MonadHost IOSync m, MonadReplace m, MonadHold m, MonadIOSync m) => MonadWidget m
