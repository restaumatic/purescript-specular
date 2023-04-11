module Bench.Primitives
  ( dynamicTests
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
import Specular.FRP (Dynamic, holdDyn, map2, newDynamic, newEvent)
import Specular.FRP.Base (subscribeDyn_)
import Specular.Ref as Ref
import Test.Utils.Dom (T3(..))

dynamicTests :: Tests
dynamicTests =
  createTests <>
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

createTests :: Tests
createTests = do
  let pureunit = pure unit
  T3 op_name op sizes<-
    [ T3 "map" (map identity) [1,2,10,20,100,1000]
    , T3 "bind_l" (_ >>= pure)  [1,2,10,20,100]
    , T3 "bind_r" (\x -> pureunit >>= \_ -> x) [1,2,10,20,100]
    , T3 "map2" (map2 const pureunit) [1,2,10,20,100,1000]
    ]
  n <- sizes
  [ Tuple ("create_" <> op_name <> "_" <> show n) (create op n)
  , Tuple ("create_sub_" <> op_name <> "_" <> show n) (create_sub op n)
  ]

  where
  create op n = do
    ref <- Ref.new unit
    pure do
      pure unit
      let _ = foldr ($) (Ref.value ref) $ Array.replicate n op
      pure unit

  create_sub op n = do
    ref <- Ref.new unit
    pure do
      pure unit
      let d = foldr ($) (Ref.value ref) $ Array.replicate n op
      void $ runHost $ subscribeDyn_ (\_ -> pure unit) d


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

type Host = CleanupT Effect

runHost :: forall a. Host a -> Effect a
runHost = map fst <<< runCleanupT
