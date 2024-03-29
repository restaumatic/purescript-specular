module Examples.Counter (spec, mainWidget) where

import Prelude hiding (append)

import Data.Tuple (Tuple(..))
import Specular.Dom.Browser (innerHTML)
import Specular.Dom.Builder.Class (dynText, el, text)
import Specular.Dom.Node.Class ((:=))
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (class MonadFRP, Dynamic, Event, foldDyn, leftmost)
import Specular.FRP.Fix (fixFRP)
import Specular.FRP.WeakDynamic (WeakDynamic)
import Test.Spec (Spec, describe, it)
import Test.Utils (liftEffect, shouldReturn)
import Test.Utils.Dom (runBuilderInDiv, dispatchTrivialEvent, querySelector)

spec :: Spec Unit
spec = describe "Counter" $ do
  it "initially renders zero" $ do
    Tuple node _ <- runBuilderInDiv mainWidget

    liftEffect (innerHTML node) `shouldReturn`
      ( """<p>0</p>"""
          <> """<button class="increment">Increment</button>"""
          <>
            """<button class="decrement">Decrement</button>"""
      )

  it "reacts to increment/decrement buttons" $ do
    Tuple node _ <- runBuilderInDiv mainWidget

    incrementButton <- liftEffect $ querySelector ".increment" node
    decrementButton <- liftEffect $ querySelector ".decrement" node

    liftEffect $ dispatchTrivialEvent incrementButton "click"

    liftEffect (innerHTML node) `shouldReturn`
      ( """<p>1</p>"""
          <> """<button class="increment">Increment</button>"""
          <>
            """<button class="decrement">Decrement</button>"""
      )

    liftEffect $ dispatchTrivialEvent decrementButton "click"
    liftEffect $ dispatchTrivialEvent decrementButton "click"
    liftEffect $ dispatchTrivialEvent decrementButton "click"

    liftEffect (innerHTML node) `shouldReturn`
      ( """<p>-2</p>"""
          <> """<button class="increment">Increment</button>"""
          <>
            """<button class="decrement">Decrement</button>"""
      )

mainWidget :: forall m. MonadWidget m => m Unit
mainWidget = fixFRP $ view >=> control

view
  :: forall m
   . MonadWidget m
  => { value :: WeakDynamic Int }
  -> m
       { increment :: Event Unit
       , decrement :: Event Unit
       }
view { value } = do
  el "p" $ dynText (show <$> value)

  increment <- buttonOnClick (pure $ "class" := "increment") $ text "Increment"
  decrement <- buttonOnClick (pure $ "class" := "decrement") $ text "Decrement"

  pure { increment, decrement }

control
  :: forall m
   . MonadFRP m
  => { increment :: Event Unit
     , decrement :: Event Unit
     }
  -> m
       ( Tuple
           { value :: Dynamic Int }
           Unit
       )
control { increment, decrement } = do
  value <- foldDyn ($) 0 $
    leftmost
      [ (_ + 1) <$ increment
      , (_ - 1) <$ decrement
      ]
  pure (Tuple { value } unit)
