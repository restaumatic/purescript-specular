module Specular.Lumiform.Lumiform where

import Prelude (class Functor, Unit, discard, pure, unit, ($))

import Control.Monad.Free (Free, liftF, runFreeM)
import Effect (Effect)
import Effect.Class.Console (log)

-- DSL
data LumiformF a =
    Output String a
  | Bell a
  | Done a

type Lumiform = Free LumiformF

instance functorLumiformF :: Functor LumiformF where
  map f (Output s a) = Output s (f a)
  map f (Bell a) = Bell (f a)
  map f (Done a) = Done (f a)

output :: String -> Lumiform Unit
output str = liftF $ Output str unit

bell :: Lumiform Unit
bell = liftF $ Bell unit

done :: Lumiform Unit
done = liftF $ Done unit

lumiform :: forall a . Lumiform a -> Effect a
lumiform = runFreeM go
  where
    go (Output str a) = do
      log str
      pure a
    go (Bell a) = do
      log "!"
      pure a
    go (Done a) = do
      log "DONE"
      pure a