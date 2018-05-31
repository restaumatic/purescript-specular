module InputWidgetsSpec where

import Prelude hiding (append)

import Control.Monad.Cleanup (runCleanupT)
import Data.Foreign (toForeign)
import Specular.Internal.Effect (newRef)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Specular.Dom.Widgets.Input (getTextInputValue, setTextInputValue, textInput, textInputValue, textInputValueEventOnEnter)
import Specular.FRP (never, newEvent)
import Specular.FRP.Base (subscribeDyn_, subscribeEvent_)
import Test.Spec (Spec, describe, it, pending')
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (append, ioSync, shouldHaveValue, shouldReturn)
import Test.Utils.Dom (dispatchEvent, dispatchTrivialEvent, querySelector, runBuilderInDiv)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "Input widgets" $ do
  describe "textInput" $ do
    let makeTextInput config = do
          Tuple div widget <- runBuilderInDiv $ textInput config
          node <- ioSync $ querySelector "input" div
          pure {node,widget}

    it "sets initial value" $ do
      {node} <- makeTextInput
        { initialValue: "foo", setValue: never, attributes: pure mempty }
      ioSync (getTextInputValue node) `shouldReturn` "foo"

    it "changes value on event" $ do
      {event,fire} <- ioSync newEvent
      {node} <- makeTextInput
        { initialValue: "foo", setValue: event, attributes: pure mempty }

      ioSync $ fire "bar"
      ioSync (getTextInputValue node) `shouldReturn` "bar"

    it "return value changes when setValue fires" $ do
      {event,fire} <- ioSync newEvent
      log <- ioSync $ newRef []
      {node,widget} <- makeTextInput
        { initialValue: "initial", setValue: event, attributes: pure mempty }

      ioSync $ do
        void $ runCleanupT $ subscribeDyn_ (append log) (textInputValue widget)
        fire "setValue"
        setTextInputValue node "oninput"
        dispatchTrivialEvent node "input"

      log `shouldHaveValue` ["initial", "setValue", "oninput"]

    pending' "textInputValueEventOnEnter" $ do
      -- FIXME: unable to simulate the keypress event correctly
      log <- ioSync $ newRef []
      {node,widget} <- makeTextInput
        { initialValue: "initial", setValue: never, attributes: pure mempty }
      void $ ioSync $ runCleanupT $ do
        event <- textInputValueEventOnEnter widget
        subscribeEvent_ (append log) event

      ioSync $ do
        setTextInputValue node "changed1"
        dispatchEvent node "keypress" (toForeign { key: "A" })

        setTextInputValue node "changed2"
        dispatchEvent node "keypress" (toForeign { key: "Enter" })
      
      log `shouldHaveValue` ["changed2"]
