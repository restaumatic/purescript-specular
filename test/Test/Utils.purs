module Test.Utils where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.IOSync (IOSync, runIOSync)
import Data.Array (snoc)
import Data.IORef (IORef, modifyIORef, readIORef, writeIORef)
import Test.Spec.Assertions (shouldEqual)

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
