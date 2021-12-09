module InputWidgetsSpec where

import Prelude hiding (append)

import Control.Monad.Cleanup (runCleanupT)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (new)
import Foreign (unsafeToForeign)
import Specular.Dom.Browser (Node)
import Specular.Dom.Widgets.Input (checkboxView, getCheckboxChecked, getTextInputValue, setTextInputValue, textInput, textInputValue, textInputValueEventOnEnter, textInputValueOnChange)
import Specular.FRP (holdDyn, never, newEvent, weaken)
import Specular.FRP.Base (subscribeDyn_, subscribeEvent_)
import Test.Spec (Spec, describe, it, pending')
import Test.Utils (append, liftEffect, shouldHaveValue, shouldReturn)
import Test.Utils.Dom (dispatchEvent, dispatchTrivialEvent, querySelector, runBuilderInDiv)

spec :: Spec Unit
spec = describe "Input widgets" $ do
  describe "textInput" $ do
    let
      makeTextInput config = do
        Tuple div widget <- runBuilderInDiv $ textInput config
        node <- liftEffect $ querySelector "input" div
        pure { node, widget }

    it "sets initial value" $ do
      { node } <- makeTextInput
        { initialValue: "foo", setValue: never, attributes: pure mempty }
      liftEffect (getTextInputValue node) `shouldReturn` "foo"

    it "changes value on event" $ do
      { event, fire } <- liftEffect newEvent
      { node } <- makeTextInput
        { initialValue: "foo", setValue: event, attributes: pure mempty }

      liftEffect $ fire "bar"
      liftEffect (getTextInputValue node) `shouldReturn` "bar"

    it "return value changes when setValue fires" $ do
      { event, fire } <- liftEffect newEvent
      log <- liftEffect $ new []
      { node, widget } <- makeTextInput
        { initialValue: "initial", setValue: event, attributes: pure mempty }

      liftEffect $ do
        void $ runCleanupT $ subscribeDyn_ (append log) (textInputValue widget)
        fire "setValue"
        setTextInputValue node "oninput"
        dispatchTrivialEvent node "input"

      log `shouldHaveValue` [ "initial", "setValue", "oninput" ]

    describe "textInputValueOnChange" do
      it "return value changes when setValue fires" do
        { event, fire } <- liftEffect newEvent
        log <- liftEffect $ new []
        { node, widget } <- makeTextInput
          { initialValue: "initial", setValue: event, attributes: pure mempty }

        liftEffect $ do
          void $ runCleanupT $ (textInputValueOnChange widget) >>= subscribeDyn_ (append log)
          fire "setValue"
          setTextInputValue node "onchange"
          dispatchTrivialEvent node "change"

        log `shouldHaveValue` [ "initial", "setValue", "onchange" ]

    pending' "textInputValueEventOnEnter" $ do
      -- FIXME: unable to simulate the keypress event correctly
      log <- liftEffect $ new []
      { node, widget } <- makeTextInput
        { initialValue: "initial", setValue: never, attributes: pure mempty }
      void $ liftEffect $ runCleanupT $ do
        event <- textInputValueEventOnEnter widget
        subscribeEvent_ (append log) event

      liftEffect $ do
        setTextInputValue node "changed1"
        dispatchEvent node "keypress" (unsafeToForeign { key: "A" })

        setTextInputValue node "changed2"
        dispatchEvent node "keypress" (unsafeToForeign { key: "Enter" })

      log `shouldHaveValue` [ "changed2" ]

  describe "checkboxView" $ do
    let
      makeCheckbox value = do
        Tuple div event <- runBuilderInDiv $ checkboxView value (pure mempty)
        node <- liftEffect $ querySelector "input" div
        pure { node, event }
    it "sets initial value (false)" do
      { node, event: _event } <- makeCheckbox (pure false)
      liftEffect (getCheckboxChecked node) `shouldReturn` false
    it "sets initial value (true)" do
      { node, event: _event } <- makeCheckbox (pure true)
      liftEffect (getCheckboxChecked node) `shouldReturn` true
    it "handle external update when not touched by the user" $ do
      { event: changeValueEvt, fire: changeValue } <- liftEffect newEvent
      Tuple valueDyn _ <- runCleanupT $ holdDyn false changeValueEvt
      { node, event: _event } <- makeCheckbox (weaken valueDyn)
      liftEffect (getCheckboxChecked node) `shouldReturn` false
      liftEffect $ changeValue true
      liftEffect (getCheckboxChecked node) `shouldReturn` true
      liftEffect $ changeValue false
      liftEffect (getCheckboxChecked node) `shouldReturn` false
    it "handle external update after touched by the user" $ do
      { event: changeValueEvt, fire: changeValue } <- liftEffect newEvent
      Tuple valueDyn _ <- runCleanupT $ holdDyn false changeValueEvt
      { node, event: _event } <- makeCheckbox (weaken valueDyn)
      liftEffect (getCheckboxChecked node) `shouldReturn` false
      liftEffect $ triggerNodeClicked node
      liftEffect (getCheckboxChecked node) `shouldReturn` true
      liftEffect $ changeValue false
      liftEffect (getCheckboxChecked node) `shouldReturn` false
      liftEffect $ changeValue true
      liftEffect (getCheckboxChecked node) `shouldReturn` true

foreign import triggerNodeClicked :: Node -> Effect Unit -- for some reason dispatchTrivialEvent node "click" didn't work
