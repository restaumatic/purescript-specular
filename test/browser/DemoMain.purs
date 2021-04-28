module DemoMain where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Examples.AsyncRequest as AsyncRequest
import Examples.Counter as Counter
import Examples.CounterRef as CounterRef
import Examples.Radio as Radio
import Examples.RegistrationForm as RegistrationForm
import Specular.Dom.Builder.Class (el, text)
import Specular.Dom.Widget (class MonadWidget, Widget, runMainWidgetInBody)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (Event, holdDyn, leftmost, weakDynamic_)
import Specular.FRP.Fix (fixFRP)

newtype Demo = Demo
  { name :: String
  , run :: Unit -> Widget Unit
  }

runDemo :: Demo -> Widget Unit
runDemo (Demo {run}) = run unit

demos :: Array Demo
demos =
  [ Demo { name: "Counter", run: \_ -> Counter.mainWidget }
  , Demo { name: "RegistrationForm", run: \_ -> void RegistrationForm.mainWidget }
  , Demo { name: "AsyncRequest", run: \_ -> AsyncRequest.mainWidget }
  , Demo { name: "Radio", run: \_ -> Radio.mainWidget }
  , Demo { name: "CounterRef", run: \_ -> CounterRef.mainWidget }
  ]

demoButton :: forall m. MonadWidget m => Demo -> m (Event Demo)
demoButton demo@(Demo {name}) = do
  clicked <- buttonOnClick (pure mempty) (text name)
  pure $ demo <$ clicked

mainWidget :: Widget Unit
mainWidget = fixFRP $ \view -> do
  weakDynamic_ $ view.currentDemo <#> \m_demo ->
    case m_demo of
      Nothing -> text "(no demo chosen)"

      Just (Demo {name, run}) -> do
        el "h2" $ text $ "Current demo: " <> name
        run unit


  el "h2" $ text "Choose another demo:"
  changeDemo <- leftmost <$> traverse demoButton demos

  currentDemo <- holdDyn Nothing (map Just changeDemo)

  pure (Tuple {currentDemo} unit)

main :: Effect Unit
main = runMainWidgetInBody mainWidget
