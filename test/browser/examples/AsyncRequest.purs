module Examples.AsyncRequest (spec, mainWidget) where

import Prelude hiding (append)

import BuilderSpec (newDynamic)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, delay)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Specular.Dom.Browser (innerHTML)
import Specular.Dom.Builder.Class (el, text)
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Input (textInputOnInput)
import Specular.FRP (Dynamic, current, pull, readBehavior, weakDynamic_)
import Specular.FRP.Async (RequestState(Loaded, Loading, NotRequested), asyncRequestMaybe)
import Specular.FRP.Fix (fixFRP)
import Specular.FRP.WeakDynamic (WeakDynamic)
import Specular.Internal.Effect (newRef, readRef, writeRef)
import Test.Spec (Spec, describe, it)
import Test.Utils (shouldReturn, yieldAff)
import Test.Utils.Dom (runBuilderInDiv)

spec :: Spec Unit
spec = describe "AsyncRequest" $ do
  it "initially renders empty form and empty result" $ do
    Tuple node _ <- runBuilderInDiv (mainWidgetWith instantBackend)

    liftEffect (innerHTML node) `shouldReturn`
      ( """<div><label>Input: </label><input></div>""" <>
        """<div></div>"""
      )

  describe "logic" $ do
    it "renders request state" $ do
      avar <- AVar.empty
      let backend = { toUpper: \_ -> AVar.take avar }
      Tuple query setQuery <- liftEffect $ newDynamic ""

      Tuple _ (Tuple {result} _) <- runBuilderInDiv $ control backend {query}
      liftEffect (pull $ readBehavior $ current result) `shouldReturn` NotRequested

      liftEffect $ setQuery "foo"
      yieldAff
      liftEffect (pull $ readBehavior $ current result) `shouldReturn` Loading

      AVar.put "FOO" avar
      liftEffect (pull $ readBehavior $ current result) `shouldReturn` Loaded "FOO"

    it "always displays the latest request" $ do
      firstRequest <- AVar.empty
      secondRequest <- AVar.empty
      currentRequestVar <- liftEffect $ newRef firstRequest
      let backend = { toUpper: \_ -> do
                        var <- liftEffect $ readRef currentRequestVar
                        AVar.take var
                    }
      Tuple query setQuery <- liftEffect $ newDynamic ""

      Tuple _ (Tuple {result} _) <- runBuilderInDiv $ control backend {query}

      liftEffect $ setQuery "foo"
      liftEffect $ writeRef currentRequestVar secondRequest
      liftEffect $ setQuery "bar"

      AVar.put "FOO" firstRequest
      yieldAff
      liftEffect (pull $ readBehavior $ current result) `shouldReturn` Loading

      AVar.put "BAR" secondRequest
      yieldAff
      liftEffect (pull $ readBehavior $ current result) `shouldReturn` Loaded "BAR"


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

mainWidget :: forall m. MonadWidget m => m Unit
mainWidget = mainWidgetWith slowBackend

mainWidgetWith :: forall m. MonadWidget m => Backend -> m Unit
mainWidgetWith backend = fixFRP $ view >=> control backend

view :: forall m. MonadWidget m
  => { result :: WeakDynamic (RequestState String) }
  -> m { query :: Dynamic String }
view {result} = do
  query <- el "div" $ do
    el "label" $ text "Input: "
    textInputOnInput "" mempty

  el "div" $ do
    weakDynamic_ $ flip map result $
      case _ of
        NotRequested -> pure unit
        Loading -> text $ "Accessing Webscale Uppercase Service..."
        Loaded x -> text $ "Result is: " <> x

  pure { query }

control :: forall m. MonadWidget m
  => Backend
  -> { query :: Dynamic String }
  -> m (Tuple
    { result :: Dynamic (RequestState String) }
    Unit
    )
control backend {query} = do
  result <- asyncRequestMaybe $
    map (\s -> if s == "" then Nothing else Just (backend.toUpper s)) query
  pure $ Tuple { result } unit
