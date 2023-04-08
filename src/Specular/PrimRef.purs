module Specular.PrimRef
  ( PrimRef(..)
  , new
  , value
  , modify
  , read
  , write
  ) where

import Prelude hiding (const)

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (runEffectFn1, runEffectFn2)
import Specular.FRP.Base(Dynamic(..), stabilize)
import Specular.Internal.Incremental as I
import Specular.Internal.Incremental.Node as Node

newtype PrimRef a = PrimRef (I.Var a)

-- | Create a new PrimRef with an initial value.
new :: forall m a. MonadEffect m => a -> m (PrimRef a)
new x = liftEffect do
  v <- runEffectFn1 I.newVar x
  pure (PrimRef v)

-- | The current value of the PrimRef, as a Dynamic.
value :: forall a. PrimRef a -> Dynamic a
value (PrimRef v) = Dynamic (I.readVar v)

-- | Modify value of this PrimRef using a function.
modify :: forall a m. MonadEffect m => PrimRef a -> (a -> a) -> m Unit
modify (PrimRef v) f = liftEffect do
  x <- runEffectFn1 Node.valueExc (I.readVar v)
  runEffectFn2 I.setVar v (f x)
  stabilize

-- | Overwrite value of this PrimRef.
write :: forall a m. MonadEffect m => PrimRef a -> a -> m Unit
write (PrimRef v) x = liftEffect do
  runEffectFn2 I.setVar v x
  stabilize

-- | Read the current value of a PrimRef
read :: forall m a. MonadEffect m => PrimRef a -> m a
read (PrimRef v) = liftEffect $ runEffectFn1 Node.valueExc (I.readVar v)
