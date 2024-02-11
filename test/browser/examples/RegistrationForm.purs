module Examples.RegistrationForm (spec, mainWidget) where

import Prelude hiding (append)

import Control.Monad.Cleanup (runCleanupT)
import Data.Tuple (Tuple(..))
import Effect.Ref (new)
import Specular.Dom.Browser (innerHTML)
import Specular.Dom.Element (attr, bindValueOnChange, class_, el, el_, onClick_, text)
import Specular.Dom.Widget (Widget)
import Specular.FRP (Dynamic, Event, newEvent, readDynamic, unlessD, whenD)
import Specular.FRP.Base (subscribeEvent_)
import Specular.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Utils (append, liftEffect, shouldHaveValue, shouldReturn)
import Test.Utils.Dom (dispatchTrivialEvent, querySelector, runBuilderInDiv, setInputValueWithChange)

spec :: Spec Unit
spec = describe "RegistrationForm" do
  it "initially renders empty form" do
    Tuple node _ <- runBuilderInDiv mainWidget

    liftEffect (innerHTML node) `shouldReturn`
      ( """<div><label>Login: </label><input class="login"></div>"""
          <> """<div><label>Password: </label><input type="password" class="password"></div>"""
          <> """<div><label>Repeat password: </label><input type="password" class="repeat-password"></div>"""
          <>
            """<button>Register</button>"""
      )

  it "reacts to password change" do
    Tuple node _ <- runBuilderInDiv mainWidget

    passwordInput <- liftEffect $ querySelector ".password" node
    liftEffect $ setInputValueWithChange "foo" passwordInput

    -- NB: Input values are not present in innerHTML
    liftEffect (innerHTML node) `shouldReturn`
      ( """<div><label>Login: </label><input class="login"></div>"""
          <> """<div><label>Password: </label><input type="password" class="password"></div>"""
          <> """<div><label>Repeat password: </label><input type="password" class="repeat-password"></div>"""
          <> """<div>Passwords do not match</div>"""
          <>
            """<button>Register</button>"""
      )

  it "reacts to submit button" do
    let showFormResult { login, password } = "login: " <> login <> ", password: " <> password

    Tuple node event <- runBuilderInDiv mainWidget
    log <- liftEffect $ new []
    _ <- liftEffect $ runCleanupT $ subscribeEvent_ (append log <<< showFormResult) event

    loginInput <- liftEffect $ querySelector ".login" node
    liftEffect $ setInputValueWithChange "user" loginInput

    passwordInput <- liftEffect $ querySelector ".password" node
    repeatPasswordInput <- liftEffect $ querySelector ".repeat-password" node
    liftEffect $ setInputValueWithChange "hunter2" passwordInput
    liftEffect $ setInputValueWithChange "hunter2" repeatPasswordInput

    submitButton <- liftEffect $ querySelector "button" node
    liftEffect $ dispatchTrivialEvent submitButton "click"

    log `shouldHaveValue` [ "login: user, password: hunter2" ]

-- | Data obtained from the form.
type FormResult =
  { login :: String
  , password :: String
  }

-- | Renders a registration form.
-- | Returns an Event that fires on "Register" button,
-- | with the form data.
mainWidget :: Widget (Event FormResult)
mainWidget = do
  login <- Ref.new ""
  password <- Ref.new ""
  repeatPassword <- Ref.new ""

  result <- newEvent

  let
    loginIsTaken = map (_ == "admin") (Ref.value login)
    passwordsMatch = (==) <$> Ref.value password <*> Ref.value repeatPassword

    -- FIXME: This should be replaced by `rsequence`
    formResult :: Dynamic FormResult
    formResult =  do
      loginValue <- Ref.value login
      passwordValue <- Ref.value password
      pure { login: loginValue, password: passwordValue }

    register = do
      value <- readDynamic formResult
      result.fire value

  el_ "div" do
    el_ "label" $ text "Login: "
    el "input" [class_ "login", bindValueOnChange login] (pure unit)
    whenD loginIsTaken do
      text "Login already taken"

  el_ "div" do
    el_ "label" $ text "Password: "
    el "input" [attr "type" "password" , class_ "password", bindValueOnChange password] (pure unit)

  el_ "div" do
    el_ "label" $ text "Repeat password: "
    el "input" [attr "type" "password" , class_ "repeat-password", bindValueOnChange repeatPassword] (pure unit)

  unlessD passwordsMatch do
      el_ "div" $ text "Passwords do not match"

  el "button" [onClick_ register] do
    text "Register"

  pure result.event
