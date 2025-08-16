module Specular.Dom.Element.Class where

import Prelude

import Data.Tuple (Tuple(..), snd)
import Effect (foreachE)
import Effect.Uncurried (mkEffectFn2, runEffectFn2)
import Specular.Dom.Browser (Node, TagName, appendChild, createElement)
import Specular.Dom.Builder.Class (liftBuilderWithRun)
import Specular.Dom.Element (Prop(..))
import Specular.Dom.Widget (class MonadWidget)

el' :: forall m a. MonadWidget m => TagName -> Array Prop -> m a -> m (Tuple Node a)
el' tagName props body = liftBuilderWithRun
  ( mkEffectFn2 \env run -> do
      node <- createElement tagName
      result <- runEffectFn2 run (env { parent = node }) body
      foreachE props \(Prop prop) ->
        runEffectFn2 prop node env.cleanup
      appendChild node env.parent
      pure (Tuple node result)
  )

el :: forall m a. MonadWidget m => TagName -> Array Prop -> m a -> m a
el tagName props body = snd <$> el' tagName props body

el_ :: forall m. MonadWidget m => TagName -> Array Prop -> m Unit
el_ tagName props = el tagName props (pure unit)
