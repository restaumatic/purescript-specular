module Specular.Dom.Builder.Class where

import Prelude

import Control.Monad.Cleanup (onCleanup)
import Control.Monad.IOSync (IOSync)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Replace (class MonadReplace)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Tuple (Tuple, snd)
import Specular.Dom.Node.Class (class EventDOM, Attrs, EventType, Namespace, TagName, addEventListener)
import Specular.FRP (class MonadFRP, Event, WeakDynamic, hostEffect, newEvent, weakDynamic_)

class Monad m <= MonadDomBuilder node m | m -> node where
  text :: String -> m Unit
  dynText :: WeakDynamic String -> m Unit
  elDynAttrNS' :: forall a. Maybe Namespace -> TagName -> WeakDynamic Attrs -> m a -> m (Tuple node a)
  rawHtml :: String -> m Unit

  elAttr :: forall a. TagName -> Attrs -> m a -> m a

elDynAttr'
  :: forall m node a. MonadDomBuilder node m
  => String -> WeakDynamic Attrs -> m a -> m (Tuple node a)
elDynAttr' = elDynAttrNS' Nothing

elDynAttr ::
     forall node m a
   . MonadDomBuilder node m
  => String
  -> WeakDynamic Attrs
  -> m a
  -> m a
elDynAttr tagName dynAttrs inner = snd <$> elDynAttr' tagName dynAttrs inner


elAttr' ::
     forall node m a
   . MonadDomBuilder node m
  => String
  -> Attrs
  -> m a
  -> m (Tuple node a)
elAttr' tagName attrs inner = elDynAttr' tagName (pure attrs) inner


el' ::
     forall node m a
   . MonadDomBuilder node m
  => String
  -> m a
  -> m (Tuple node a)
el' tagName inner = elAttr' tagName mempty inner


el ::
     forall node m a
   . MonadDomBuilder node m
  => String
  -> m a
  -> m a
el tagName inner = elAttr tagName mempty inner


dynRawHtml ::
     forall node m
   . MonadDomBuilder node m
  => MonadReplace m
  => MonadFRP m
  => WeakDynamic String
  -> m Unit
dynRawHtml dynHtml = weakDynamic_ (rawHtml <$> dynHtml)


domEventWithSample ::
     forall event node m a
   . EventDOM event node
  => MonadFRP m
  => (event -> IOSync a)
  -> EventType
  -> node
  -> m (Event a)
domEventWithSample sample eventType node = do
  {event,fire} <- newEvent
  unsub <- hostEffect $ addEventListener eventType (sample >=> fire) node
  onCleanup unsub
  pure event


domEvent ::
     forall event node m
   . EventDOM event node
  => MonadFRP m
  => EventType
  -> node
  -> m (Event Unit)
domEvent = domEventWithSample (\_ -> pure unit)

instance monadDomBuilderReaderT :: MonadDomBuilder node m => MonadDomBuilder node (ReaderT r m) where
  text = lift <<< text
  dynText = lift <<< dynText
  elDynAttrNS' ns tag attrs body = ReaderT $ \env -> elDynAttrNS' ns tag attrs $ runReaderT body env
  rawHtml = lift <<< rawHtml
  elAttr tag attrs body =
    ReaderT $ \env -> elAttr tag attrs $ runReaderT body env

class MonadDetach m where
  -- | Initialize a widget without displaying it immediately.
  -- | Returns the `value` and a monadic action (`widget`) to display the widget.
  -- |
  -- | When the `widget` computation is executed twice, the widget should only
  -- | appear in the latest place it is displayed.
  detach :: forall a. m a -> m { value :: a, widget :: m Unit }

instance monadDetachReaderT :: (Monad m, MonadDetach m) => MonadDetach (ReaderT r m) where
  detach inner = ReaderT $ \env -> do
    { value, widget } <- detach $ runReaderT inner env
    pure { value, widget: lift widget }
