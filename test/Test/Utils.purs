module Test.Utils where

import Prelude hiding (append)

import Control.Monad.Aff (Aff, Milliseconds(..), delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.IOSync (IOSync, runIOSync)
import Data.Array (snoc)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Test.Spec.Assertions (fail, shouldEqual)
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

newtype SpyIO a = SpyIO
  { fn :: a -> IOSync Unit
  , values :: IORef (Array a)
  }

newSpyIO :: forall r a. Aff r (SpyIO a)
newSpyIO = do
  log <- ioSync $ newIORef []
  pure $ SpyIO { fn: append log , values: log }

trigger :: forall a b. SpyIO a -> a -> b -> IOSync b
trigger (SpyIO spy) x y = spy.fn x *> pure y

assertValues :: forall r a. Eq a => Show a => SpyIO a -> Array a -> Aff r Unit
assertValues (SpyIO spy) = shouldHaveValue spy.values

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

-- | Reschedule the current fiber to the end of event loop.
-- | Equivalent to `setTimeout(function() { ... }, 0);`
yieldAff :: forall e. Aff e Unit
yieldAff = delay (Milliseconds 0.0)

foreign import getTotalListeners :: forall e. Eff e Int
foreign import modifyTotalListeners :: forall e. (Int -> Int) -> Eff e Unit

withLeakCheck :: forall e a. Aff e a -> Aff e a
withLeakCheck = withLeakCheck' ""

withLeakCheck' :: forall e a. String -> Aff e a -> Aff e a
withLeakCheck' msg action = do
  totalBefore <- liftEff getTotalListeners
  result <- action
  totalAfter <- liftEff getTotalListeners
  let msg' = if msg == "" then "" else " (" <> msg <> ")"
  when (totalBefore /= totalAfter) $
    fail $ "Subscriber leak" <> msg' <> "! listeners before=" <> show totalBefore <> " after=" <> show totalAfter
  pure result
