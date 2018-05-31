module Specular.Internal.RIO
  ( RIO(..)
  , INF
  , rio
  , runRIO
  , local
  ) where

import Prelude

import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Uncurried (EffFn1)
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.IOSync (IOSync)
import Control.Monad.IOSync.Class (class MonadIOSync)
import Control.Monad.Reader.Class (class MonadAsk)
import Unsafe.Coerce (unsafeCoerce)

type INF = (infinity :: INFINITY)

newtype RIO r a = RIO (EffFn1 INF r a)

instance functorRIO :: Functor (RIO r) where
  map = mapImpl

instance applyRIO :: Apply (RIO r) where
  apply = applyImpl

instance applicativeRIO :: Applicative (RIO r) where
  pure = pureImpl

instance bindRIO :: Bind (RIO r) where
  bind = bindImpl

instance monadRIO :: Monad (RIO r)

instance monadAskRIO :: MonadAsk r (RIO r) where
  ask = askImpl

instance monadEffRIO :: MonadEff eff (RIO r) where
  liftEff = unsafeCoerce

instance monadIOSyncRIO :: MonadIOSync (RIO r) where
  liftIOSync = unsafeCoerce

foreign import pureImpl  :: forall r a.   a                                -> RIO r a
foreign import mapImpl   :: forall r a b. (a -> b)       -> RIO r a        -> RIO r b
foreign import applyImpl :: forall r a b. RIO r (a -> b) -> RIO r a        -> RIO r b
foreign import bindImpl  :: forall r a b. RIO r a        -> (a -> RIO r b) -> RIO r b
foreign import askImpl   :: forall r. RIO r r

foreign import runRIO :: forall r a. r -> RIO r a -> IOSync a
foreign import rio :: forall r a. (r -> IOSync a) -> RIO r a
foreign import local :: forall r e a. (e -> r) -> RIO r a -> RIO e a
