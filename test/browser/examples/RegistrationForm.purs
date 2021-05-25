module Examples.RegistrationForm (spec, mainWidget) where

import Prelude hiding (append)

import Control.Monad.Cleanup (class MonadCleanup, runCleanupT)
import Effect.Class (class MonadEffect)
import Effect.Ref (new)
import Data.Tuple (Tuple(..))
import Specular.Dom.Browser (innerHTML)
import Specular.Dom.Builder.Class (el, text)
import Specular.Dom.Node.Class ((:=))
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (textInputOnChange)
import Specular.FRP (Dynamic, Event, tagDyn, weakDynamic_)
import Specular.FRP.Base (subscribeEvent_)
import Specular.FRP.Fix (fixFRP)
import Specular.FRP.WeakDynamic (WeakDynamic)
import Test.Spec (Spec, describe, it)
import Test.Utils (append, liftEffect, shouldHaveValue, shouldReturn)
import Test.Utils.Dom (dispatchTrivialEvent, querySelector, runBuilderInDiv, setInputValueWithChange)

spec :: Spec Unit
spec = describe "RegistrationForm" $ do
  it "initially renders empty form" $ do
    Tuple node _ <- runBuilderInDiv mainWidget

    liftEffect (innerHTML node) `shouldReturn`
      ( """<div><label>Login: </label><input class="login"></div>""" <>
        """<div><label>Password: </label><input class="password" type="password"></div>""" <>
        """<div><label>Repeat password: </label><input class="repeat-password" type="password"></div>""" <>
        """<button>Register</button>"""
      )

  it "reacts to password change" $ do
    Tuple node _ <- runBuilderInDiv mainWidget

    passwordInput <- liftEffect $ querySelector ".password" node
    liftEffect $ setInputValueWithChange "foo" passwordInput

    -- NB: Input values are not present in innerHTML
    liftEffect (innerHTML node) `shouldReturn`
      ( """<div><label>Login: </label><input class="login"></div>""" <>
        """<div><label>Password: </label><input class="password" type="password"></div>""" <>
        """<div><label>Repeat password: </label><input class="repeat-password" type="password"></div>""" <>
        """<div>Passwords do not match</div>""" <>
        """<button>Register</button>"""
      )

  it "reacts to submit button" $ do
    let showFormResult {login,password} = "login: " <> login <> ", password: " <> password

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
mainWidget :: forall m. MonadWidget m => m (Event FormResult)
mainWidget = fixFRP $ view >=> control

view :: forall m. MonadWidget m
  => { loginIsTaken :: WeakDynamic Boolean
     , passwordsMatch :: WeakDynamic Boolean
     }
  -> m
    { login :: Dynamic String
    , password :: Dynamic String
    , repeatPassword :: Dynamic String
    , register :: Event Unit
    }
view {loginIsTaken, passwordsMatch} = do
  login <- el "div" $ do
    el "label" $ text "Login: "
    value <- textInputOnChange "" ("class" := "login")
    weakDynamic_ $ flip map loginIsTaken $ \loginIsTakenValue ->
      when loginIsTakenValue $
        text "Login already taken"
    pure value

  password <- el "div" $ do
    el "label" $ text "Password: "
    textInputOnChange "" ("type" := "password" <> "class" := "password")

  repeatPassword <- el "div" $ do
    el "label" $ text "Repeat password: "
    textInputOnChange "" ("type" := "password" <> "class" := "repeat-password")

  weakDynamic_ $ flip map passwordsMatch $ \passwordsMatchValue ->
    unless passwordsMatchValue $
      el "div" $ text "Passwords do not match"

  register <- buttonOnClick (pure mempty) $ text "Register"

  pure { login, password, repeatPassword, register }

control ::
     forall m
   . MonadEffect m
  => MonadCleanup m
  => { login :: Dynamic String
     , password :: Dynamic String
     , repeatPassword :: Dynamic String
     , register :: Event Unit
     }
  -> m (Tuple
    { loginIsTaken :: Dynamic Boolean
    , passwordsMatch :: Dynamic Boolean
    }
    (Event FormResult)
    )
control {login,password,repeatPassword,register: registerButtonClicked} = do
  let
    loginIsTaken = map (_ == "admin") login
    passwordsMatch = (==) <$> password <*> repeatPassword

    register = tagDyn formResult registerButtonClicked
    
    -- FIXME: This should be replaced by `rsequence`
    formResult :: Dynamic FormResult
    formResult = do
       loginValue <- login
       passwordValue <- password
       pure { login: loginValue, password: passwordValue }

  pure $ Tuple
    {loginIsTaken, passwordsMatch}
    register
