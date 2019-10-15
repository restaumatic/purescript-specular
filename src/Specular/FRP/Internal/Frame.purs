module Specular.FRP.Internal.Frame where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Cleanup (class MonadCleanup, onCleanup)
import Control.Monad.Reader (ask)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (cons, unsnoc)
import Data.Array as Array
import Data.Foldable (for_)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn2, mkEffectFn2, runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Specular.Internal.Effect (DelayedEffects, Ref, emptyDelayed, modifyRef, newRef, pushDelayed, readRef, sequenceEffects, unsafeFreezeDelayed, writeRef)
import Specular.Internal.RIO (RIO, rio, runRIO)
import Specular.Internal.RIO as RIO
import Specular.Internal.UniqueMap.Mutable as UMM

-------------------------------------------------

-- | Logical time.
-- There's no monotonicity requirement (for now), so we have only Eq instance.
newtype Time = Time Int

derive newtype instance eqTime :: Eq Time

-- | Pull is a computation that reads a value given current time.
-- |
-- | Invariant: Pull computations are always idempotent (`forall x :: Pull a. x *> x = x`).
newtype Pull a = MkPull (RIO Time a)

runPull :: forall a. Time -> Pull a -> Effect a
runPull time (MkPull x) = runRIO time x

derive newtype instance functorPull :: Functor Pull
derive newtype instance applyPull :: Apply Pull
derive newtype instance applicativePull :: Applicative Pull
derive newtype instance bindPull :: Bind Pull
derive newtype instance monadPull :: Monad Pull



-- | Computations that occur during a Frame.
--
-- During a frame, no arbitrary effects are performed. Instead they are
-- registered using `effect` to be performed after the frame.
--
-- Frame computations have access to current logical time. See `oncePerFrame`
-- for why this is needed.
newtype Frame a = Frame (RIO FrameEnv a)

type FrameEnv =
  { effects :: DelayedEffects
  , time :: Time
  }



derive newtype instance functorFrame :: Functor Frame
derive newtype instance applyFrame :: Apply Frame
derive newtype instance applicativeFrame :: Applicative Frame
derive newtype instance bindFrame :: Bind Frame
derive newtype instance monadFrame :: Monad Frame
