module Bench.Primitives ( dynamicTests) where

import Prelude

import Bench.Types (Tests)
import Control.Monad.Cleanup (CleanupT, runCleanupT)
import Data.Array (range, replicate)
import Data.Foldable (for_, sum)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Specular.FRP (Dynamic, holdDyn, never, newDynamic, newEvent)
import Specular.FRP.Base (subscribeDyn_)

dynamicTests :: Tests
dynamicTests =
  [ Tuple "10 subscribers" $ nsubscribers 10
  , Tuple "20 subscribers" $ nsubscribers 20
  , Tuple "30 subscribers" $ nsubscribers 30
  , Tuple "40 subscribers" $ nsubscribers 40
  ] <>
  [ Tuple "dyn" $ testDynFn1 pure
  , Tuple "dyn fmap" $ testDynFn1 \d -> pure (add 1 <$> d)
  , Tuple "dyn ap pure" $ testDynFn1 \d -> pure (pure (const 1) <*> d)
  , Tuple "dyn ap self" $ testDynFn1 \d -> pure (add <$> d <*> d)
  , Tuple "dyn bind self" $ testDynFn1 \d -> pure (d >>= \_ -> d)
  , Tuple "dyn bind inner" $ testDynFn1 \d -> pure (pure 10 >>= \_ -> d)
  , Tuple "dyn bind outer" $ testDynFn1 \d -> pure (d >>= \_ -> pure 10)
  ] <>
  nestedApplyTests

nestedApplyTests :: Tests
nestedApplyTests =
  [ Tuple "dyn 1ap  - fire first" $ test_n_ap_first 1
  , Tuple "dyn 5ap  - fire first" $ test_n_ap_first 5
  , Tuple "dyn 10ap - fire first" $ test_n_ap_first 10
  , Tuple "dyn 15ap - fire first" $ test_n_ap_first 15

  , Tuple "dyn 1ap  - fire last" $ test_n_ap_last 1
  , Tuple "dyn 5ap  - fire last" $ test_n_ap_last 5
  , Tuple "dyn 10ap - fire last" $ test_n_ap_last 10
  , Tuple "dyn 15ap - fire last" $ test_n_ap_last 15
  ]

  where
  test_n_ap_first n =
    testDynFn1 \d -> do
      dynamics <- sequence $ replicate n do
        event <- newEvent
        holdDyn 0 event.event
      pure $ map sum $ sequence ([d] <> dynamics)

  test_n_ap_last n =
    testDynFn1 \d -> do
      dynamics <- sequence $ replicate n do
        event <- newEvent
        holdDyn 0 event.event
      pure $ map sum $ sequence (dynamics <> [d])

nsubscribers :: Int -> Effect (Effect Unit)
nsubscribers n =
  runHost do
    dyn <- newDynamic 0
    for_ (range 0 n) \_ ->
      subscribeDyn_ (\_ -> pure unit) dyn.dynamic
    pure (dyn.set 1)

testDynFn1 :: (Dynamic Int -> Host (Dynamic Int)) -> Effect (Effect Unit)
testDynFn1 fn =
  runHost do
    event <- newEvent
    dyn <- holdDyn 0 event.event
    dyn' <- fn dyn
    subscribeDyn_ (\_ -> pure unit) dyn'
    pure (event.fire 1)

testDynFn2 :: (Dynamic Int -> Dynamic Int -> Host (Dynamic Int)) -> Effect (Effect Unit)
testDynFn2 fn =
  runHost do
    event <- newEvent
    dyn <- holdDyn 0 event.event
    dyn2 <- holdDyn 0 never
    dyn' <- fn dyn dyn2
    subscribeDyn_ (\_ -> pure unit) dyn'
    pure (event.fire 1)

type Host = CleanupT Effect

runHost :: forall a. Host a -> Effect a
runHost = map fst <<< runCleanupT
