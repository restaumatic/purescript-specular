module Test.Utils
  ( module Test.Utils
  , module Effect.Class
  ) where

import Prelude hiding (append)

import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Array (snoc)
import Specular.Internal.Effect (Ref, modifyRef, newRef, readRef, writeRef)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Prelude (Proxy)
import Effect.Uncurried (runEffectFn1)
import Specular.Internal.Incremental.Global (globalTotalRefcount)
import Specular.Internal.Incremental.Ref as Ref

append :: forall a. Ref (Array a) -> a -> Effect Unit
append ref value = modifyRef ref (\a -> snoc a value)

clear :: forall a. Ref (Array a) -> Aff Unit
clear ref = liftEffect $ writeRef ref []

shouldHaveValue :: forall a. Eq a => Show a => Ref a -> a -> Aff Unit
shouldHaveValue ref expected = liftEffect (readRef ref) `shouldReturn` expected

shouldReturn :: forall t. Show t => Eq t => Aff t -> t -> Aff Unit
shouldReturn action expected = do
  actual <- action
  actual `shouldEqual` expected

newtype SpyIO a = SpyIO
  { fn :: a -> Effect Unit
  , values :: Ref (Array a)
  }

newSpyIO :: forall a. Aff (SpyIO a)
newSpyIO = do
  log <- liftEffect $ newRef []
  pure $ SpyIO { fn: append log , values: log }

trigger :: forall a b. SpyIO a -> a -> b -> Effect b
trigger (SpyIO spy) x y = spy.fn x *> pure y

assertValues :: forall a. Eq a => Show a => SpyIO a -> Array a -> Aff Unit
assertValues (SpyIO spy) = shouldHaveValue spy.values

class ShouldHaveInferredType :: forall k. Type -> k -> Constraint
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
yieldAff :: Aff Unit
yieldAff = delay (Milliseconds 0.0)

getTotalListeners :: Effect Int
getTotalListeners = runEffectFn1 Ref.read globalTotalRefcount

withLeakCheck :: forall a. Aff a -> Aff a
withLeakCheck = withLeakCheck' ""

withLeakCheck' :: forall a. String -> Aff a -> Aff a
withLeakCheck' msg action = do
  totalBefore <- liftEffect getTotalListeners
  result <- action
  totalAfter <- liftEffect getTotalListeners
  let msg' = if msg == "" then "" else " (" <> msg <> ")"
  when (totalBefore /= totalAfter) $
    fail $ "Subscriber leak" <> msg' <> "! listeners before=" <> show totalBefore <> " after=" <> show totalAfter
  pure result
