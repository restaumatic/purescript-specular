module BenchMain where

import Prelude

import Bench.Builder (builderTests)
import Bench.Primitives (dynamicTests, weakDynamicTests)
import Bench.Types (Tests)
import Data.List.Lazy (replicateM)
import Data.Traversable (for)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Console as Console

main :: Effect Unit
main = do
  bench builderTests
  bench dynamicTests
  bench weakDynamicTests

bench :: Tests -> Effect Unit
bench tests = do

  Console.log "Warmup..."

  tests' <- for tests $ \(Tuple name setupFn) -> do
    fn <- setupFn
    void $ replicateM 100 fn
    pure (Tuple name fn)

  Console.log "Benchmarking..."

  runBenchmark $ map (\(Tuple name fn) -> { name, fn }) tests'

foreign import runBenchmark :: Array { name :: String, fn :: Effect Unit } -> Effect Unit
