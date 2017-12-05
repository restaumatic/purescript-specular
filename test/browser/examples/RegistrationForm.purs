module Examples.RegistrationForm (spec, mainWidget) where

import Prelude hiding (append)

import Control.Monad.Cleanup (class MonadCleanup, runCleanupT)
import Control.Monad.IOSync.Class (class MonadIOSync)
import Data.IORef (newIORef)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Specular.Dom.Browser (innerHTML)
import Specular.Dom.Builder.Class (class MonadWidget, el, text)
import Specular.Dom.Node.Class ((:=))
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (textInputOnChange)
import Specular.FRP (Dynamic, Event, tagDyn, weakDynamic_)
import Specular.FRP.Base (subscribeEvent_)
import Specular.FRP.Fix (fixFRP)
import Specular.FRP.WeakDynamic (WeakDynamic)
import Test.Spec (Spec, describe, it)
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (append, ioSync, shouldHaveValue, shouldReturn)
import Test.Utils.Dom (dispatchTrivialEvent, querySelector, runBuilderInDiv, setInputValueWithChange)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "RegistrationForm" $ do
  it "initially renders empty form" $ do
    Tuple node _ <- runBuilderInDiv mainWidget

    ioSync (innerHTML node) `shouldReturn`
      ( """<div><label>Login: </label><input value="" class="login"></div>""" <>
        """<div><label>Password: </label><input value="" class="password" type="password"></div>""" <>
        """<div><label>Repeat password: </label><input value="" class="repeat-password" type="password"></div>""" <>
        """<button>Register</button>"""
      )

  it "reacts to password change" $ do
    Tuple node _ <- runBuilderInDiv mainWidget

    passwordInput <- ioSync $ querySelector ".password" node
    ioSync $ setInputValueWithChange "foo" passwordInput

    -- NB: Input values are not present in innerHTML
    ioSync (innerHTML node) `shouldReturn`
      ( """<div><label>Login: </label><input value="" class="login"></div>""" <>
        """<div><label>Password: </label><input value="" class="password" type="password"></div>""" <>
        """<div><label>Repeat password: </label><input value="" class="repeat-password" type="password"></div>""" <>
        """<div>Passwords do not match</div>""" <>
        """<button>Register</button>"""
      )

  it "reacts to submit button" $ do
    let showFormResult {login,password} = "login: " <> login <> ", password: " <> password

    Tuple node event <- runBuilderInDiv mainWidget
    log <- ioSync $ newIORef []
    _ <- ioSync $ runCleanupT $ subscribeEvent_ (append log <<< showFormResult) event


    loginInput <- ioSync $ querySelector ".login" node
    ioSync $ setInputValueWithChange "user" loginInput

    passwordInput <- ioSync $ querySelector ".password" node
    repeatPasswordInput <- ioSync $ querySelector ".repeat-password" node
    ioSync $ setInputValueWithChange "hunter2" passwordInput
    ioSync $ setInputValueWithChange "hunter2" repeatPasswordInput

    submitButton <- ioSync $ querySelector "button" node
    ioSync $ dispatchTrivialEvent submitButton "click"

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
   . MonadIOSync m
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
