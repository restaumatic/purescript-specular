module BenchMain where

import Prelude

import Benchmark (fnEff, runBench)
import Benchmark.Suite.Monad (runSuiteM)
import Benchmark.Suite.ST (new)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.IOSync (IOSync, runIOSync')
import Control.Monad.ST (ST)
import Data.List.Lazy (replicateM)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder.Class (elAttr, text)
import Specular.Dom.Node.Class (createElement, (:=))
import Specular.Dom.Widget (class MonadWidget, Widget, runWidgetInNode)
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

  staticJS_m 10

  log "Warmup..."
  for_ tests $ \(Tuple name fn) ->
    void $ replicateM 5000 fn

  log "Benchmarking..."
  bench

tests =
  [ Tuple "js 10" (staticJS 10)
  , Tuple "js_c 10" (staticJS_c 10)
  , Tuple "js_m 10" (staticJS_m 10)
  , Tuple "static mono 10" (runWidget $ staticWidgetMono 10)
  , Tuple "static 10" (runWidget $ deoptimizeWidget (staticWidget 10))
  ]

bench = do
  runBench $
    for_ tests $ \(Tuple name fn) ->
      fnEff name fn
