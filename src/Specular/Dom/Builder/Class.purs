module Specular.Dom.Builder.Class where

import Prelude

import Control.Monad.Cleanup (onCleanup)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Replace (class MonadReplace)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn2, runEffectFn2)
import Specular.Dom.Node.Class (class EventDOM, Attrs, EventType, Namespace, TagName, addEventListener)
import Specular.FRP (class MonadFRP, Event, WeakDynamic, newEvent, weakDynamic_)
import Specular.Internal.Effect (DelayedEffects)

type BuilderEnv node =
  { parent :: node
  , cleanup :: DelayedEffects
  }

class Monad m <= MonadDomBuilder node m | m -> node where
  text :: String -> m Unit
  dynText :: WeakDynamic String -> m Unit
  elDynAttrNS' :: forall a. Maybe Namespace -> TagName -> WeakDynamic Attrs -> m a -> m (Tuple node a)
  rawHtml :: String -> m Unit

  elAttr :: forall a. TagName -> Attrs -> m a -> m a

  liftBuilder :: forall a. EffectFn1 (BuilderEnv node) a -> m a
  liftBuilderWithRun :: forall a b. EffectFn2 (BuilderEnv node) (EffectFn2 (BuilderEnv node) (m b) b) a -> m a

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

elAttr_ ::
     forall node m
   . MonadDomBuilder node m
  => String
  -> Attrs
  -> m Unit
elAttr_ tagName attrs = elAttr tagName attrs (pure unit)


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

el_ ::
     forall node m
   . MonadDomBuilder node m
  => String
  -> m Unit
el_ tagName = el tagName (pure unit)


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
  => (event -> Effect a)
  -> EventType
  -> node
  -> m (Event a)
domEventWithSample sample eventType node = do
  {event,fire} <- newEvent
  onDomEvent eventType node (sample >=> fire)
  pure event


domEvent ::
     forall event node m
   . EventDOM event node
  => MonadFRP m
  => EventType
  -> node
  -> m (Event Unit)
domEvent = domEventWithSample (\_ -> pure unit)


-- | Register a DOM event listener.
onDomEvent
  :: forall m node event
   . EventDOM event node
  => MonadFRP m
  => EventType
  -> node
  -> (event -> Effect Unit)
  -> m Unit
onDomEvent eventType node handler = do
  unsub <- liftEffect $ addEventListener eventType handler node
  onCleanup unsub

instance monadDomBuilderReaderT :: MonadDomBuilder node m => MonadDomBuilder node (ReaderT r m) where
  text = lift <<< text
  dynText = lift <<< dynText
  elDynAttrNS' ns tag attrs body = ReaderT $ \env -> elDynAttrNS' ns tag attrs $ runReaderT body env
  rawHtml = lift <<< rawHtml
  elAttr tag attrs body =
    ReaderT $ \env -> elAttr tag attrs $ runReaderT body env
  liftBuilder = lift <<< liftBuilder
  liftBuilderWithRun fn = ReaderT \e ->
    liftBuilderWithRun (mkEffectFn2 \benv run ->
      runEffectFn2 fn benv (mkEffectFn2 \benv' m ->
        runEffectFn2 run benv' (runReaderT m e)))

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
