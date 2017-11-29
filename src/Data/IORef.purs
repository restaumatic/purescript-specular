module Data.IORef where

import Prelude

import Control.Monad.IOSync (IOSync)

foreign import data IORef :: Type -> Type

foreign import newIORef :: forall a. a -> IOSync (IORef a)

foreign import readIORef :: forall a. IORef a -> IOSync a

foreign import writeIORef :: forall a. IORef a -> a -> IOSync Unit

modifyIORef :: forall a. IORef a -> (a -> a) -> IOSync Unit
modifyIORef ref f = do
  value <- readIORef ref
  writeIORef ref (f value)
