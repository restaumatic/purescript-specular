module Test.Utils.Dom where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.IOSync (IOSync)
import Data.Foreign (Foreign, toForeign)
import Data.Tuple (Tuple(..))
import Specular.Dom.Browser (Node)
import Specular.Dom.Node.Class (EventType, createElement)
import Specular.Dom.Widget (Widget, runWidgetInNode)
import Test.Utils (ioSync)

data T3 a b c = T3 a b c

runBuilderInDiv :: forall r a. Widget a -> Aff r (Tuple Node a)
runBuilderInDiv widget = ioSync $ do
  parent <- createElement "div"
  Tuple result unsub <- runWidgetInNode parent widget
  pure (Tuple parent result)

runBuilderInDiv' :: forall r a. Widget a -> Aff r (T3 Node a (IOSync Unit))
runBuilderInDiv' widget = ioSync $ do
  parent <- createElement "div"
  Tuple result unsub <- runWidgetInNode parent widget
  pure (T3 parent result unsub)

-- | Find a node matching the given selector in the parent node.
-- | Crashes if node not found.
foreign import querySelector :: String -> Node -> IOSync Node

-- | Dispatch an Event with the given type on the node
foreign import dispatchEvent :: Node -> EventType -> Foreign -> IOSync Unit

-- | Set input value and dispatch "change" event.
foreign import setInputValueWithChange :: String -> Node -> IOSync Unit

dispatchTrivialEvent :: Node -> EventType -> IOSync Unit
dispatchTrivialEvent node eventType = dispatchEvent node eventType (toForeign {})
