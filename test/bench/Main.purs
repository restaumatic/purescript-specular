module BenchMain where

import Prelude

import Benchmark (fnEff, runBench)
import Control.Monad.Cleanup (CleanupT, runCleanupT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.IOSync (IOSync, runIOSync, runIOSync')
import Control.Monad.ST (ST)
import Data.List.Lazy (replicateM)
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..), fst)
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder.Class (elAttr, text)
import Specular.Dom.Node.Class (createElement, (:=))
import Specular.Dom.Widget (class MonadWidget, Widget, runWidgetInNode)
import Specular.FRP (Dynamic, holdDyn, never, newEvent)
import Specular.FRP.Base (subscribeDyn_)
import Test.Utils.Dom (T3(T3))

foreign import exportBenchmark :: forall e. Eff e Unit

runBuilderInDiv' :: forall a. Widget a -> IOSync (T3 Node a (IOSync Unit))
runBuilderInDiv' widget = do
  parent <- createElement "div"
  Tuple result unsub <- runWidgetInNode parent widget
  pure (T3 parent result unsub)

runWidget :: forall a e. Widget a -> Eff (infinity :: INFINITY | e) Unit
runWidget w = void $ runIOSync' $ runBuilderInDiv' w

deoptimizeWidget :: forall a. (forall m. MonadWidget m => m a) -> Widget a
deoptimizeWidget x = x

staticWidget :: forall m. MonadWidget m => Int -> m Unit
staticWidget n =
  void $ replicateM n $
    elAttr "div" ("class" := "foo") $ do
      elAttr "div" ("class" := "bar") $ do
        text "foo"
      elAttr "div" ("class" := "baz") $ do
        text "foo"
      elAttr "div" ("class" := "thud") $ do
        text "foo"

staticWidgetMono :: Int -> Widget Unit
staticWidgetMono n =
  void $ replicateM n $
    elAttr "div" ("class" := "foo") $ do
      elAttr "div" ("class" := "bar") $ do
        text "foo"
      elAttr "div" ("class" := "baz") $ do
        text "foo"
      elAttr "div" ("class" := "thud") $ do
        text "foo"

foreign import staticJS :: forall e. Int -> Eff e Unit
foreign import staticJS_c :: forall e. Int -> Eff e Unit
foreign import staticJS_m :: forall e. Int -> Eff e Unit

main :: forall s. Eff (st :: ST s, console :: CONSOLE, infinity :: INFINITY) Unit
main = do
  exportBenchmark

  log "Warmup..."
  for_ tests $ \(Tuple name fn) ->
    void $ replicateM 5000 fn

  log "Benchmarking..."
  bench

type Test e = Tuple String (Eff (infinity :: INFINITY | e) (Eff (infinity :: INFINITY | e) Unit))
type Tests = forall e. Array (Test e)

tests :: Tests
tests =
  builderTests <> 
  dynamicTests <>
  []

builderTests :: Tests
builderTests =
  [ Tuple "js 10" (pure $ staticJS 10)
  , Tuple "js_c 10" (pure $ staticJS_c 10)
  , Tuple "js_m 10" (pure $ staticJS_m 10)
  , Tuple "static mono 10" (pure $ runWidget $ staticWidgetMono 10)
  , Tuple "static 10" (pure $ runWidget $ deoptimizeWidget (staticWidget 10))
  ]

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

bench :: forall s. Eff (st :: ST s, console :: CONSOLE, infinity :: INFINITY) Unit
bench = do
  tests' <- for tests $ \(Tuple name fn) ->
    Tuple name <$> fn
  runBench $
    for tests' $ \(Tuple name fn) ->
      fnEff name fn
