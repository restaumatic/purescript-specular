module Specular.Dom.Element
  ( module Specular.Dom.Element
  , module X
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Effect (foreachE)
import Effect.Uncurried (EffectFn2, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2)
import Foreign.Object as Object
import Specular.Dom.Browser (Node, appendChild)
import Specular.Dom.Builder.Class (liftBuilderWithRun)
import Specular.Dom.Builder.Class (text, rawHtml) as X
import Specular.Dom.Node.Class (Attrs, TagName, createElement, removeAttributes, setAttributes)
import Specular.Dom.Widget (class MonadWidget)
import Specular.FRP (Dynamic, readDynamic, _subscribeEvent, changed)
import Specular.Internal.Effect (DelayedEffects, newRef, readRef, writeRef, pushDelayed)

newtype Prop = Prop (EffectFn2 Node DelayedEffects Unit)

instance semigroupProp :: Semigroup Prop where
  append (Prop fn1) (Prop fn2) = Prop $ mkEffectFn2 \x y -> do
    runEffectFn2 fn1 x y
    runEffectFn2 fn2 x y

instance monoidProp :: Monoid Prop where
  mempty = Prop $ mkEffectFn2 \_ _ -> pure unit

el' :: forall m a. MonadWidget m => TagName -> Array Prop -> m a -> m (Tuple Node a)
el' tagName props body = liftBuilderWithRun $ mkEffectFn2 \env run -> do
  node <- createElement tagName
  result <- runEffectFn2 run (env { parent = node }) body
  foreachE props \(Prop prop) ->
    runEffectFn2 prop node env.cleanup
  appendChild node env.parent
  pure (Tuple node result)

el :: forall m a. MonadWidget m => TagName -> Array Prop -> m a -> m a
el tagName props body = snd <$> el' tagName props body

el_ :: forall m. MonadWidget m => TagName -> Array Prop -> m Unit
el_ tagName props = el tagName props (pure unit)

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
