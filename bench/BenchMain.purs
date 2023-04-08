module BenchMain where

import Prelude

import Bench.Builder (builderTests)
import Bench.Primitives (dynamicTests, weakDynamicTests)
import Bench.Types (Tests)
import Data.Traversable (for)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console

main :: Effect Unit
main = launchAff_ do
--  bench "Builder" builderTests
  bench "Dynamic" dynamicTests
--  bench "WeakDynamic" weakDynamicTests

  Console.log "done"

bench :: String -> Tests -> Aff Unit
bench suiteName tests = do
  delay (Milliseconds 10.0)

  Console.log $ "------ " <> suiteName

  tests' <- for tests $ \(Tuple name setupFn) -> do
    liftEffect do
      fn <- setupFn
      pure (Tuple name fn)

  liftEffect $ runBenchmark $ map (\(Tuple name fn) -> { name, fn }) tests'

foreign import runBenchmark :: Array { name :: String, fn :: Effect Unit } -> Effect Unit
