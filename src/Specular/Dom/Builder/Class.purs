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
import Specular.Dom.Browser (Attrs, EventType, Namespace, Node, TagName, addEventListener)
import Specular.Dom.Browser as DOM
import Specular.FRP (class MonadFRP, WeakDynamic, newEvent, weakDynamic_)
import Specular.FRP as FRP
import Specular.Internal.Effect (DelayedEffects)

type BuilderEnv env =
  { parent :: Node
  , cleanup :: DelayedEffects
  , userEnv :: env
  }

class Monad m <= MonadDomBuilder m where
  liftBuilder :: forall a. (forall env. EffectFn1 (BuilderEnv env) a) -> m a
  liftBuilderWithRun :: forall a b. (forall env. EffectFn2 (BuilderEnv env) (EffectFn2 (BuilderEnv env) (m b) b) a) -> m a

-- | Register a DOM event listener.
onDomEvent :: forall m. MonadFRP m => EventType -> Node -> (DOM.Event -> Effect Unit) -> m Unit
onDomEvent eventType node handler = do
  unsub <- liftEffect $ addEventListener eventType handler node
  onCleanup unsub

instance MonadDomBuilder m => MonadDomBuilder (ReaderT r m) where
  liftBuilder b = lift (liftBuilder b)
  liftBuilderWithRun fn = ReaderT \e ->
    liftBuilderWithRun
      ( mkEffectFn2 \benv run ->
          runEffectFn2 fn benv
            ( mkEffectFn2 \benv' m ->
                runEffectFn2 run benv' (runReaderT m e)
            )
      )
