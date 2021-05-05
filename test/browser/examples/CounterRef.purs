module Examples.CounterRef (spec, mainWidget) where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Specular.Dom.Browser (innerHTML)
import Specular.Dom.Element (attr, class_, el, el_, onClick_, text, dynText)
import Specular.Dom.Widget (class MonadWidget, Widget, runMainWidgetInBody)
import Specular.Ref (Ref)
import Specular.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Utils (liftEffect, shouldReturn)
import Test.Utils.Dom (runBuilderInDiv, dispatchTrivialEvent, querySelector)




spec :: Spec Unit
spec = describe "Counter" $ do
  it "initially renders zero" $ do
    Tuple node _ <- runBuilderInDiv mainWidget

    liftEffect (innerHTML node) `shouldReturn`
      ( """<p>0</p>""" <>
        """<button class="increment" type="button">Increment</button>""" <>
        """<button class="decrement" type="button">Decrement</button>""" <>
        """<button class="reset" type="button">Reset</button>"""
      )



  it "reacts to increment/decrement buttons" $ do
    Tuple node _ <- runBuilderInDiv mainWidget

    incrementButton <- liftEffect $ querySelector ".increment" node
    decrementButton <- liftEffect $ querySelector ".decrement" node

    liftEffect $ dispatchTrivialEvent incrementButton "click"

    liftEffect (innerHTML node) `shouldReturn`
      ( """<p>1</p>""" <>
        """<button class="increment" type="button">Increment</button>""" <>
        """<button class="decrement" type="button">Decrement</button>""" <>
        """<button class="reset" type="button">Reset</button>"""
      )

    liftEffect $ dispatchTrivialEvent decrementButton "click"
    liftEffect $ dispatchTrivialEvent decrementButton "click"
    liftEffect $ dispatchTrivialEvent decrementButton "click"

    liftEffect (innerHTML node) `shouldReturn`
      ( """<p>-2</p>""" <>
        """<button class="increment" type="button">Increment</button>""" <>
        """<button class="decrement" type="button">Decrement</button>""" <>
        """<button class="reset" type="button">Reset</button>"""
      )

  it "it can reset to zero" $ do
    Tuple node _ <- runBuilderInDiv mainWidget

    incrementButton <- liftEffect $ querySelector ".increment" node
    resetButton <- liftEffect $ querySelector ".reset" node

    liftEffect $ dispatchTrivialEvent incrementButton "click"
    liftEffect $ dispatchTrivialEvent incrementButton "click"
    liftEffect $ dispatchTrivialEvent resetButton "click"

    liftEffect (innerHTML node) `shouldReturn`
      ( """<p>0</p>""" <>
        """<button class="increment" type="button">Increment</button>""" <>
        """<button class="decrement" type="button">Decrement</button>""" <>
        """<button class="reset" type="button">Reset</button>"""
      )


mainWidget :: Widget Unit
mainWidget = do
    counter :: Ref Int <- Ref.new 0
  
    -- | Subtract 1 from counter value
    let subtractCb = (Ref.modify counter) (add (negate 1))

    -- | Add 1 to counter value
    let addCb =  (Ref.modify counter) (add 1)

    -- | Reset counter to 0
    let reset =  Ref.write counter 0

    el_ "p" $ dynText $ show <$> Ref.value counter

    el "button" [class_ "increment", attr "type" "button", onClick_ addCb ] do
       text "Increment"

    el "button" [class_ "decrement", attr "type" "button", onClick_ subtractCb ] do
      text "Decrement"

    el "button" [class_ "reset", attr "type" "button", onClick_ reset] do
      text "Reset"
