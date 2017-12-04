module Test.Utils.Dom where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Tuple (Tuple(..))
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder (Builder, runBuilder)
import Specular.Dom.Node.Class (createElement)
import Test.Utils (ioSync)

runBuilderInDiv :: forall r a. Builder Node a -> Aff r (Tuple Node a)
runBuilderInDiv builder = ioSync $ do
  parent <- createElement "div"
  Tuple result _ <- runBuilder {parent} builder
  pure (Tuple parent result)
