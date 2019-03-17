module BenchMain where

import Prelude

import Bench.Builder (builderTests)
import Bench.Primitives (dynamicTests, weakDynamicTests)
import Bench.Types (Tests)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect, forE)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console

main :: Effect Unit
main = launchAff_ do
  bench "Builder" builderTests
  bench "Dynamic" dynamicTests
  bench "WeakDynamic" weakDynamicTests

bench :: String -> Tests -> Aff Unit
bench suiteName tests = do

  Console.log $ "------ " <> suiteName

  Console.log "Warmup..."

  tests' <- for tests $ \(Tuple name setupFn) -> do
    delay (Milliseconds 100.0)
    liftEffect do
      fn <- setupFn
      forE 0 1000 \_ -> fn
      pure (Tuple name fn)

  Console.log "Benchmarking..."

  liftEffect $ forE 0 2 \_ -> runBenchmark $ map (\(Tuple name fn) -> { name, fn }) tests'

foreign import runBenchmark :: Array { name :: String, fn :: Effect Unit } -> Effect Unit
