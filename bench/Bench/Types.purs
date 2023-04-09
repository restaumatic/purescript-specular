module Bench.Types
  ( Tests
  , Test
  , prefix
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)

type Test = Tuple String (Effect (Effect Unit))
type Tests = Array Test

prefix :: String -> Tests -> Tests
prefix p = map (\(Tuple name t) -> Tuple (p <> name) t)
