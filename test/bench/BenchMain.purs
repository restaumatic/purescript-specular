module BenchMain where

import Prelude

import Bench.Builder (builderTests)
import Bench.Primitives (dynamicTests, weakDynamicTests)
import Bench.Types (Tests)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.ST (ST)
import Data.List.Lazy (replicateM)
import Data.Traversable (for)
import Data.Tuple (Tuple(Tuple))

main :: forall s. Eff (st :: ST s, console :: CONSOLE, infinity :: INFINITY) Unit
main = do
  bench builderTests
  bench dynamicTests
  bench weakDynamicTests

bench :: forall s. Tests -> Eff (st :: ST s, console :: CONSOLE, infinity :: INFINITY) Unit
bench tests = do

  log "Warmup..."

  tests' <- for tests $ \(Tuple name setupFn) -> do
    fn <- setupFn
    void $ replicateM 100 fn
    pure (Tuple name fn)

  log "Benchmarking..."

  runBenchmark $ map (\(Tuple name fn) -> { name, fn }) tests'

foreign import runBenchmark
  :: forall e. Array { name :: String, fn :: Eff (infinity :: INFINITY | e) Unit }
  -> Eff (infinity :: INFINITY | e) Unit
