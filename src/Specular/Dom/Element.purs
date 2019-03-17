module Specular.Dom.Element where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (foreachE)
import Effect.Uncurried (EffectFn2, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2)
import Foreign.Object as Object
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder.Class (liftBuilderWithRun)
import Specular.Dom.Node.Class (Attrs, TagName, createElement, removeAttributes, setAttributes)
import Specular.Dom.Widget (class MonadWidget)
import Specular.FRP (Dynamic, readDynamic, _subscribeEvent, changed)
import Specular.Internal.Effect (DelayedEffects, newRef, readRef, writeRef, pushDelayed)

newtype Prop = Prop (EffectFn2 Node DelayedEffects Unit)

el :: forall m a. MonadWidget m => TagName -> Array Prop -> m a -> m a
el tagName props body = liftBuilderWithRun $ mkEffectFn2 \env run -> do
  node <- createElement tagName
  foreachE props \(Prop prop) ->
    runEffectFn2 prop node env.cleanup
  runEffectFn2 run (env { parent = node }) body

attr :: Attrs -> Prop
attr attrs = Prop $ mkEffectFn2 \node _ ->
  setAttributes node attrs

attrD :: Dynamic Attrs -> Prop
attrD dynAttrs = Prop $ mkEffectFn2 \node cleanups -> do
  attrsRef <- newRef Object.empty
  let
    resetAttributes = mkEffectFn1 \newAttrs -> do
      oldAttrs <- readRef attrsRef
      writeRef attrsRef newAttrs
      let
        changed = Object.filterWithKey (\k v -> Object.lookup k oldAttrs /= Just v) newAttrs
        removed = Array.filter (\k -> not (k `Object.member` newAttrs)) $ Object.keys oldAttrs

      removeAttributes node removed
      setAttributes node changed

  initialAttrs <- readDynamic dynAttrs
  runEffectFn1 resetAttributes initialAttrs
  unsub <- runEffectFn2 _subscribeEvent (runEffectFn1 resetAttributes) (changed dynAttrs)
  pushDelayed cleanups unsub
  pure unit
