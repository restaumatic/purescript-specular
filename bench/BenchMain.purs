module BenchMain where

import Prelude

import Bench.Builder (builderTests)
import Bench.Primitives (dynamicTests)
import Bench.Types (Tests, prefix)
import Data.Array (fold)
import Data.Traversable (for)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Class (liftEffect)

main :: Effect Unit
main = do
  bench $ fold
    [ prefix "Builder " builderTests
    , prefix "Dynamic " dynamicTests
    ]

bench ::  Tests -> Effect Unit
bench tests = do
  tests' <- for tests $ \(Tuple name setupFn) -> do
    liftEffect do
      fn <- setupFn
      pure (Tuple name fn)

  exposeRunBenchmark $ map (\(Tuple name fn) -> { name, fn }) tests'

foreign import exposeRunBenchmark :: Array { name :: String, fn :: Effect Unit } -> Effect Unit
