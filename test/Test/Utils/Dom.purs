module Test.Utils.Dom where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import Specular.Dom.Browser (Node)
import Specular.Dom.Node.Class (EventType, createElement)
import Specular.Dom.Widget (Widget, runWidgetInNode)
import Test.Utils (liftEffect)

data T3 a b c = T3 a b c

runBuilderInDiv :: forall a. Widget a -> Aff (Tuple Node a)
runBuilderInDiv widget = liftEffect do
  parent <- createElement "div"
  Tuple result _unsub <- runWidgetInNode parent widget
  pure (Tuple parent result)

runBuilderInDiv' :: forall a. Widget a -> Aff (T3 Node a (Effect Unit))
runBuilderInDiv' widget = liftEffect do
  parent <- createElement "div"
  Tuple result unsub <- runWidgetInNode parent widget
  pure (T3 parent result unsub)

-- | Find a node matching the given selector in the parent node.
-- | Crashes if node not found.
foreign import querySelector :: String -> Node -> Effect Node

-- | Dispatch an Event with the given type on the node
foreign import dispatchEvent :: Node -> EventType -> Foreign -> Effect Unit

-- | Set input value and dispatch "change" event.
foreign import setInputValueWithChange :: String -> Node -> Effect Unit

dispatchTrivialEvent :: Node -> EventType -> Effect Unit
dispatchTrivialEvent node eventType = dispatchEvent node eventType (unsafeToForeign {})

foreign import numChildNodes :: Node -> Effect Int
