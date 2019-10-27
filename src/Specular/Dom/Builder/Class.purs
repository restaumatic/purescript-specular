module Specular.Dom.Builder.Class where

import Prelude

import Control.Monad.Cleanup (onCleanup)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Replace (class MonadReplace)
import Control.Monad.Trans.Class (lift)
import Data.Array (filter) as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, mkEffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Foreign.Object as SM
import Specular.Dom.Browser (Attrs, EventType, Namespace, Node, TagName, addEventListener, appendRawHtml, createElementNS, createTextNode, removeAttributes, setAttributes, setText)
import Specular.Dom.Browser as DOM
import Specular.Dom.Node.Class (appendChild)
import Specular.FRP (class MonadFRP, Dynamic, WeakDynamic, _subscribeEvent, changed, newEvent, readDynamic, weakDynamic_)
import Specular.FRP as FRP
import Specular.FRP.WeakDynamic (unWeakDynamic)
import Specular.Internal.Effect (DelayedEffects, newRef, pushDelayed, readRef, writeRef)

type BuilderEnv env =
  { parent :: Node
  , cleanup :: DelayedEffects
  , userEnv :: env
  }

class Monad m <= MonadDomBuilder m where
  liftBuilder :: forall a. (forall env. EffectFn1 (BuilderEnv env) a) -> m a
  liftBuilderWithRun :: forall a b. (forall env. EffectFn2 (BuilderEnv env) (EffectFn2 (BuilderEnv env) (m b) b) a) -> m a

text :: forall m. MonadDomBuilder m => String -> m Unit
text str = liftBuilder (mkEffectFn1 \env -> do
  node <- createTextNode str
  appendChild node env.parent
  )

_subscribeDyn :: forall a. EffectFn3 DelayedEffects (Dynamic a) (EffectFn1 a Unit) Unit
_subscribeDyn = mkEffectFn3 \cleanups dyn fn -> do
  unsub <- runEffectFn2 _subscribeEvent (runEffectFn1 fn) (changed dyn)
  pushDelayed cleanups unsub
  initialValue <- readDynamic dyn
  runEffectFn1 fn initialValue

_subscribeWeakDyn :: forall a. EffectFn3 DelayedEffects (WeakDynamic a) (EffectFn1 a Unit) Unit
_subscribeWeakDyn = mkEffectFn3 \cleanups dyn fn -> do
  runEffectFn3 _subscribeDyn cleanups (unWeakDynamic dyn) $
    mkEffectFn1 \m ->
      case m of
        Nothing -> pure unit
        Just x -> runEffectFn1 fn x

dynText :: forall m. MonadDomBuilder m => WeakDynamic String -> m Unit
dynText dstr = liftBuilder (mkEffectFn1 \env -> do
  node <- createTextNode ""
  appendChild node env.parent
  runEffectFn3 _subscribeWeakDyn env.cleanup dstr (mkEffectFn1 (setText node)))

rawHtml :: forall m. MonadDomBuilder m => String -> m Unit
rawHtml html = liftBuilder (mkEffectFn1 \env -> 
  appendRawHtml html env.parent)


elDynAttrNS' :: forall m a. MonadDomBuilder m => Maybe Namespace -> TagName -> WeakDynamic Attrs -> m a -> m (Tuple Node a)
elDynAttrNS' namespace tagName dynAttrs inner = liftBuilderWithRun (mkEffectFn2 \env run -> do
  node <- createElementNS namespace tagName

  attrsRef <- newRef mempty
  let
    resetAttributes = mkEffectFn1 \newAttrs -> do
      oldAttrs <- readRef attrsRef
      writeRef attrsRef newAttrs
      let
        changed = SM.filterWithKey (\k v -> SM.lookup k oldAttrs /= Just v) newAttrs
        removed = Array.filter (\k -> not (k `SM.member` newAttrs)) $ SM.keys oldAttrs

      removeAttributes node removed
      setAttributes node changed

  runEffectFn3 _subscribeWeakDyn env.cleanup dynAttrs resetAttributes
  result <- runEffectFn2 run (env { parent = node }) inner
  appendChild node env.parent
  pure (Tuple node result))

elAttr :: forall m. MonadDomBuilder m => forall a. TagName -> Attrs -> m a -> m a
elAttr tagName attrs inner = liftBuilderWithRun (mkEffectFn2 \env run -> do
  node <- createElementNS Nothing tagName
  setAttributes node attrs
  result <- runEffectFn2 run (env { parent = node }) inner
  appendChild node env.parent
  pure result)


elDynAttr' :: forall m a. MonadDomBuilder m => String -> WeakDynamic Attrs -> m a -> m (Tuple Node a)
elDynAttr' = elDynAttrNS' Nothing

elDynAttr :: forall m a. MonadDomBuilder m => String -> WeakDynamic Attrs -> m a
  -> m a
elDynAttr tagName dynAttrs inner = snd <$> elDynAttr' tagName dynAttrs inner


elAttr' :: forall m a. MonadDomBuilder m => String -> Attrs -> m a -> m (Tuple Node a)
elAttr' tagName attrs inner = elDynAttr' tagName (pure attrs) inner

elAttr_ :: forall m. MonadDomBuilder m => String -> Attrs -> m Unit
elAttr_ tagName attrs = elAttr tagName attrs (pure unit)


el' :: forall m a. MonadDomBuilder m => String -> m a -> m (Tuple Node a)
el' tagName inner = elAttr' tagName mempty inner


el :: forall m a. MonadDomBuilder m => String -> m a -> m a
el tagName inner = elAttr tagName mempty inner

el_ :: forall m. MonadDomBuilder m => String -> m Unit
el_ tagName = el tagName (pure unit)


dynRawHtml :: forall m. MonadDomBuilder m => MonadReplace m => MonadFRP m => WeakDynamic String -> m Unit
dynRawHtml dynHtml = weakDynamic_ (rawHtml <$> dynHtml)


domEventWithSample :: forall m a. MonadFRP m => (DOM.Event -> Effect a) -> EventType -> Node -> m (FRP.Event a)
domEventWithSample sample eventType node = do
  {event,fire} <- newEvent
  onDomEvent eventType node (sample >=> fire)
  pure event


domEvent :: forall m. MonadFRP m => EventType -> Node -> m (FRP.Event Unit)
domEvent = domEventWithSample (\_ -> pure unit)

-- | Register a DOM event listener.
onDomEvent :: forall m. MonadFRP m => EventType -> Node -> (DOM.Event -> Effect Unit) -> m Unit
onDomEvent eventType node handler = do
  unsub <- liftEffect $ addEventListener eventType handler node
  onCleanup unsub

instance monadDomBuilderReaderT :: MonadDomBuilder m => MonadDomBuilder (ReaderT r m) where
  liftBuilder b = lift (liftBuilder b)
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
