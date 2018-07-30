module InputWidgetsSpec where

import Prelude hiding (append)

import Control.Monad.Cleanup (runCleanupT)
import Data.Tuple (Tuple(..))
import Foreign (unsafeToForeign)
import Specular.Dom.Widgets.Input (getTextInputValue, setTextInputValue, textInput, textInputValue, textInputValueEventOnEnter)
import Specular.FRP (never, newEvent)
import Specular.FRP.Base (subscribeDyn_, subscribeEvent_)
import Specular.Internal.Effect (newRef)
import Test.Spec (Spec, describe, it, pending')
import Test.Utils (append, liftEffect, shouldHaveValue, shouldReturn)
import Test.Utils.Dom (dispatchEvent, dispatchTrivialEvent, querySelector, runBuilderInDiv)

spec :: Spec Unit
spec = describe "Input widgets" $ do
  describe "textInput" $ do
    let makeTextInput config = do
          Tuple div widget <- runBuilderInDiv $ textInput config
          node <- liftEffect $ querySelector "input" div
          pure {node,widget}

    it "sets initial value" $ do
      {node} <- makeTextInput
        { initialValue: "foo", setValue: never, attributes: pure mempty }
      liftEffect (getTextInputValue node) `shouldReturn` "foo"

    it "changes value on event" $ do
      {event,fire} <- liftEffect newEvent
      {node} <- makeTextInput
        { initialValue: "foo", setValue: event, attributes: pure mempty }

      liftEffect $ fire "bar"
      liftEffect (getTextInputValue node) `shouldReturn` "bar"

    it "return value changes when setValue fires" $ do
      {event,fire} <- liftEffect newEvent
      log <- liftEffect $ newRef []
      {node,widget} <- makeTextInput
        { initialValue: "initial", setValue: event, attributes: pure mempty }

      liftEffect $ do
        void $ runCleanupT $ subscribeDyn_ (append log) (textInputValue widget)
        fire "setValue"
        setTextInputValue node "oninput"
        dispatchTrivialEvent node "input"

      log `shouldHaveValue` ["initial", "setValue", "oninput"]

    pending' "textInputValueEventOnEnter" $ do
      -- FIXME: unable to simulate the keypress event correctly
      log <- liftEffect $ newRef []
      {node,widget} <- makeTextInput
        { initialValue: "initial", setValue: never, attributes: pure mempty }
      void $ liftEffect $ runCleanupT $ do
        event <- textInputValueEventOnEnter widget
        subscribeEvent_ (append log) event

      liftEffect $ do
        setTextInputValue node "changed1"
        dispatchEvent node "keypress" (unsafeToForeign { key: "A" })

        setTextInputValue node "changed2"
        dispatchEvent node "keypress" (unsafeToForeign { key: "Enter" })
      
      log `shouldHaveValue` ["changed2"]
