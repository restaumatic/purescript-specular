module DemoMain where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.IOSync (runIOSync)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Examples.AsyncRequest as AsyncRequest
import Examples.Counter as Counter
import Examples.RegistrationForm as RegistrationForm
import Specular.Dom.Builder.Class (el, text)
import Specular.Dom.Widget (class MonadWidget, runMainWidgetInBody)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (Event, for, holdDyn, leftmost, weakDynamic_)
import Specular.FRP.Fix (fixFRP)

newtype Demo = Demo
  { name :: String
  , run :: forall m. MonadWidget m => Unit -> m Unit
  }

runDemo :: forall m. MonadWidget m => Demo -> m Unit
runDemo (Demo {run}) = run unit

demos :: Array Demo
demos =
  [ Demo { name: "Counter", run: \_ -> Counter.mainWidget }
  , Demo { name: "RegistrationForm", run: \_ -> void RegistrationForm.mainWidget }
  , Demo { name: "AsyncRequest", run: \_ -> AsyncRequest.mainWidget }
  ]

demoButton :: forall m. MonadWidget m => Demo -> m (Event Demo)
demoButton demo@(Demo {name}) = do
  clicked <- buttonOnClick (pure mempty) (text name)
  pure $ demo <$ clicked

mainWidget :: forall m. MonadWidget m => m Unit
mainWidget = fixFRP $ \view -> do
  weakDynamic_ $ for view.currentDemo $ \m_demo ->
    case m_demo of
      Nothing -> text "(no demo chosen)"

      Just (Demo {name, run}) -> do
        el "h2" $ text $ "Current demo: " <> name
        run unit


  el "h2" $ text "Choose another demo:"
  changeDemo <- leftmost <$> traverse demoButton demos

  currentDemo <- holdDyn Nothing (map Just changeDemo)

  pure (Tuple {currentDemo} unit)

main :: Eff (infinity :: INFINITY) Unit
main = runIOSync $ runMainWidgetInBody mainWidget
