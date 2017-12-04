module Test.Utils.Dom where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.IOSync (IOSync(..))
import Data.Tuple (Tuple(..))
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder (Builder, runBuilder)
import Specular.Dom.Node.Class (EventType, createElement)
import Test.Utils (ioSync)

runBuilderInDiv :: forall r a. Builder Node a -> Aff r (Tuple Node a)
runBuilderInDiv builder = ioSync $ do
  parent <- createElement "div"
  Tuple result _ <- runBuilder {parent} builder
  pure (Tuple parent result)

-- | Find a node matching the given selector in the parent node.
-- | Crashes if node not found.
foreign import querySelector :: String -> Node -> IOSync Node

-- | Dispatch an Event with the given type and no additional information
-- | on the node.
foreign import dispatchTrivialEvent :: Node -> EventType -> IOSync Unit
