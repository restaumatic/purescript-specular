module Bench.Primitives
  ( dynamicTests
  , weakDynamicTests
  ) where

import Prelude

import Bench.Types (Tests)
import Control.Monad.Cleanup (CleanupT, runCleanupT)
import Data.Array (foldr, range, replicate)
import Data.Array as Array
import Data.Foldable (for_, sum)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Specular.FRP (Dynamic, WeakDynamic, holdDyn, holdWeakDyn, never, newDynamic, newEvent, subscribeWeakDyn_)
import Specular.FRP.Base (subscribeDyn_)
import Specular.Ref as Ref

dynamicTests :: Tests
dynamicTests =
  createTests
    {-
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
  -}

createTests :: Tests
createTests =
  [ Tuple "create 10map" $ create_map 10
  , Tuple "create 20map" $ create_map 20
  , Tuple "create 100map" $ create_map 100
  ]

  where
        create_map n = do
          ref <- Ref.new 0
          pure do
            pure unit
            let _ = foldr ($) (Ref.value ref) $ Array.replicate n (map identity)
            pure unit

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

weakDynamicTests :: Tests
weakDynamicTests =
  [ Tuple "weak dyn" $ testWeakDynFn1 pure
  , Tuple "weak dyn fmap" $ testWeakDynFn1 \d -> pure (add 1 <$> d)
  , Tuple "weak dyn ap pure" $ testWeakDynFn1 \d -> pure (pure (const 1) <*> d)
  , Tuple "weak dyn ap self" $ testWeakDynFn1 \d -> pure (add <$> d <*> d)
  , Tuple "weak dyn bind self" $ testWeakDynFn1 \d -> pure (d >>= \_ -> d)
  , Tuple "weak dyn bind inner" $ testWeakDynFn1 \d -> pure (pure 10 >>= \_ -> d)
  , Tuple "weak dyn bind outer" $ testWeakDynFn1 \d -> pure (d >>= \_ -> pure 10)
  ]

testWeakDynFn1 :: (WeakDynamic Int -> Host (WeakDynamic Int)) -> Effect (Effect Unit)
testWeakDynFn1 fn =
  runHost do
    event <- newEvent
    dyn <- holdWeakDyn event.event
    dyn' <- fn dyn
    subscribeWeakDyn_ (\_ -> pure unit) dyn'
    pure ( event.fire 1)
