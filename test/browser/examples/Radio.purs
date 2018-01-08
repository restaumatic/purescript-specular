module Examples.Radio (mainWidget) where

import Prelude hiding (append)

import Specular.Dom.Builder.Class (dynText, el, elAttr, text)
import Specular.Dom.Node.Class ((:=))
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.RadioGroup (radioGroup)
import Specular.FRP (weaken)

mainWidget :: forall m. MonadWidget m => m Unit
mainWidget = do
  dyn <- radioGroup 
    { options: ["foo", "bar", "baz"]
    , initialValueIndex: 0
    , render: \name value input ->
        el "div" $
          input
          <* elAttr "label" ("for" := name) (text value)
    }
  el "br" (pure unit)

  text "Selected value: "
  dynText $ weaken dyn
