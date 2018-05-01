module Bench.Primitives
  ( dynamicTests
  ) where

import Prelude

import Bench.Types (Tests)
import Control.Monad.Cleanup (CleanupT, runCleanupT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.IOSync (IOSync, runIOSync)
import Data.Tuple (Tuple(..), fst)
import Specular.FRP (Dynamic, holdDyn, never, newEvent)
import Specular.FRP.Base (subscribeDyn_)

dynamicTests :: Tests
dynamicTests =
  [ Tuple "dyn" $ testDynFn1 pure
  , Tuple "dyn fmap" $ testDynFn1 \d -> pure (add 1 <$> d)
  , Tuple "dyn ap pure" $ testDynFn1 \d -> pure (pure (const 1) <*> d)
  , Tuple "dyn ap self" $ testDynFn1 \d -> pure (add <$> d <*> d)
  , Tuple "dyn bind self" $ testDynFn1 \d -> pure (d >>= \_ -> d)
  , Tuple "dyn bind inner" $ testDynFn1 \d -> pure (pure 10 >>= \_ -> d)
  , Tuple "dyn bind outer" $ testDynFn1 \d -> pure (d >>= \_ -> pure 10)
  ]

type Host = CleanupT IOSync

runIOSync'' :: forall e a. IOSync a -> Eff e a
runIOSync'' = unsafeCoerceEff <<< runIOSync

runHost :: forall e a. Host a -> Eff e a
runHost = runIOSync'' <<< map fst <<< runCleanupT

testDynFn1 :: forall e. (Dynamic Int -> Host (Dynamic Int)) -> Eff e (Eff e Unit)
testDynFn1 fn =
  runHost do
    event <- newEvent
    dyn <- holdDyn 0 event.event
    dyn' <- fn dyn
    subscribeDyn_ (\_ -> pure unit) dyn'
    pure (runIOSync'' $ event.fire 1)

testDynFn2 :: forall e. (Dynamic Int -> Dynamic Int -> Host (Dynamic Int)) -> Eff e (Eff e Unit)
testDynFn2 fn =
  runHost do
    event <- newEvent
    dyn <- holdDyn 0 event.event
    dyn2 <- holdDyn 0 never
    dyn' <- fn dyn dyn2
    subscribeDyn_ (\_ -> pure unit) dyn'
    pure (runIOSync'' $ event.fire 1)
