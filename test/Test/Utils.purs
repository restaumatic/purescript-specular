module Test.Utils where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.IOSync (IOSync, runIOSync)
import Data.Array (snoc)
import Data.IORef (IORef, modifyIORef, readIORef, writeIORef)
import Specular.Dom.Browser (Node)
import Specular.Dom.Node.Class (EventType)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (Proxy)

append :: forall a. IORef (Array a) -> a -> IOSync Unit
append ref value = modifyIORef ref (\a -> snoc a value)

clear :: forall a r. IORef (Array a) -> Aff r Unit
clear ref = ioSync $ writeIORef ref []

shouldHaveValue :: forall a r. Eq a => Show a => IORef a -> a -> Aff r Unit
shouldHaveValue ref expected = ioSync (readIORef ref) `shouldReturn` expected

shouldReturn :: forall r t. Show t => Eq t => Aff r t -> t -> Aff r Unit
shouldReturn action expected = do
  actual <- action
  actual `shouldEqual` expected

ioSync :: forall r a. IOSync a -> Aff r a
ioSync = liftEff <<< unsafeCoerceEff <<< runIOSync

-- | Dispatch an Event with the given type and no additional information
-- | on the node.
foreign import dispatchTrivialEvent :: Node -> EventType -> IOSync Unit

class ShouldHaveInferredType actual expected where
  -- | Assert that the given value has inferred type @expected@.
  -- | If the assertion turns out to be false, the call will not compile.
  -- |
  -- | For example, the following should not compile:
  -- |
  -- | ```
  -- | let _ = mempty `shouldHaveInferredType` (Proxy :: Proxy (Array Int))
  -- | ```
  -- |
  -- | While `mempty :: Array Int` typechecks, `mempty` itself has no concrete type.
  shouldHaveInferredType :: actual -> Proxy expected -> Unit

instance shouldHaveInferredTypeInstance :: ShouldHaveInferredType a a where
  shouldHaveInferredType _ _ = unit
