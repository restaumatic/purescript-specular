module Examples.AsyncRequest (spec, mainWidget) where

import Prelude hiding (append)

import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Specular.Dom.Browser (innerHTML)
import Specular.Dom.Element (bindValueOnChange, el, el_, text)
import Specular.Dom.Widget (Widget)
import Specular.FRP (withDynamic_)
import Specular.FRP.Async (RequestState(Loaded, Loading, NotRequested), asyncRequestMaybe)
import Specular.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Utils (shouldReturn)
import Test.Utils.Dom (runBuilderInDiv)

spec :: Spec Unit
spec = describe "AsyncRequest" $ do
  it "initially renders empty form and empty result" $ do
    Tuple node _ <- runBuilderInDiv (mainWidgetWith instantBackend)

    liftEffect (innerHTML node) `shouldReturn`
      ( """<div><label>Input: </label><input></div>""" <>
          """<div></div>"""
      )

instantBackend :: Backend
instantBackend = { toUpper: pure <<< String.toUpper }

slowBackend :: Backend
slowBackend = { toUpper }
  where
  toUpper s = do
    Console.log $ "Request started:  " <> show s
    delay (Milliseconds 1200.0)
    Console.log $ "Request finished: " <> show s
    pure (String.toUpper s)

type Backend =
  { toUpper :: String -> Aff String
  }

mainWidget :: Widget Unit
mainWidget = mainWidgetWith slowBackend

mainWidgetWith :: Backend -> Widget Unit
mainWidgetWith backend = do
  query <- Ref.new ""
  result <- asyncRequestMaybe $
    map (\s -> if s == "" then Nothing else Just (backend.toUpper s)) (Ref.value query)

  el_ "div" do
    el_ "label" $ text "Input: "
    el "input" [ bindValueOnChange query ] (pure unit)

  el_ "div" do
    withDynamic_ result $
      case _ of
        NotRequested -> pure unit
        Loading -> text $ "Accessing Webscale Uppercase Service..."
        Loaded x -> text $ "Result is: " <> x
