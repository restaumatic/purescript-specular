module Data.DelayedEffects (
    DelayedEffects
  , Effect
  , empty
  , push
  , unsafeFreeze
  , sequenceEffects
) where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.IOSync (IOSync)
import Data.Array.ST (STArray, emptySTArray, pushSTArray)
import Data.Array.ST as STA
import Unsafe.Coerce (unsafeCoerce)

foreign import data RealWorld :: Type

type Effect = IOSync Unit

newtype DelayedEffects = DelayedEffects (STArray RealWorld Effect)

empty :: IOSync DelayedEffects
empty = liftEff $ DelayedEffects <$> emptySTArray

push :: DelayedEffects -> Effect -> IOSync Unit
push (DelayedEffects effs) e = liftEff $ void $ pushSTArray effs e

unsafeFreeze :: DelayedEffects -> IOSync (Array Effect)
unsafeFreeze (DelayedEffects effs) = liftEff $ STA.unsafeFreeze effs

foreign import sequenceEffects :: Array Effect -> Effect
