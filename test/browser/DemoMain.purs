module DemoMain where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Examples.AsyncRequest as AsyncRequest
import Examples.CounterRef as CounterRef
import Examples.RegistrationForm as RegistrationForm
import Specular.Dom.Element (el, el_, onClick_, text)
import Specular.Dom.Widget (Widget, runMainWidgetInBody)
import Specular.FRP (withDynamic_)
import Specular.Ref as Ref

newtype Demo = Demo
  { name :: String
  , run :: Unit -> Widget Unit
  }

runDemo :: Demo -> Widget Unit
runDemo (Demo { run }) = run unit

demos :: Array Demo
demos =
  [ Demo { name: "RegistrationForm", run: \_ -> void RegistrationForm.mainWidget }
  , Demo { name: "AsyncRequest", run: \_ -> AsyncRequest.mainWidget }
  , Demo { name: "CounterRef", run: \_ -> CounterRef.mainWidget }
  ]

mainWidget :: Widget Unit
mainWidget = do
  currentDemo <- Ref.new Nothing

  withDynamic_ (Ref.value currentDemo) \m_demo ->
    case m_demo of
      Nothing -> text "(no demo chosen)"

      Just (Demo { name, run }) -> do
        el_ "h2" $ text $ "Current demo: " <> name
        run unit

  el_ "h2" $ text "Choose another demo:"
  for_ demos \demo@(Demo { name }) -> do
    el "button" [ onClick_ $ Ref.write currentDemo (Just demo) ] do
      text name

main :: Effect Unit
main = runMainWidgetInBody mainWidget
