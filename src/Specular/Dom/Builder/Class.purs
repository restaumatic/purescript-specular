module Specular.Dom.Builder.Class where

import Prelude

import Control.Monad.Cleanup (onCleanup)
import Control.Monad.IOSync (IOSync)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Monoid (mempty)
import Data.Tuple (Tuple, snd)
import Specular.Dom.Node.Class (class EventDOM, Attrs, EventType, addEventListener)
import Specular.FRP (class MonadHost, Event, WeakDynamic, hostEffect, newEvent)

class Monad m <= MonadDomBuilder node m | m -> node where
  text :: String -> m Unit
  dynText :: WeakDynamic String -> m Unit
  elDynAttr' :: forall a . String -> WeakDynamic Attrs -> m a -> m (Tuple node a)
  rawHtml :: String -> m Unit

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

instance monadDomBuilderReaderT :: MonadDomBuilder node m => MonadDomBuilder node (ReaderT r m) where
  text = lift <<< text
  dynText = lift <<< dynText
  elDynAttr' tag attrs body = ReaderT $ \env -> elDynAttr' tag attrs $ runReaderT body env
  rawHtml = lift <<< rawHtml
