module Bench.Types
  ( Tests
  , Test
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.IO.Effect (INFINITY)
import Data.Tuple (Tuple)

type Test e = Tuple String (Eff (infinity :: INFINITY | e) (Eff (infinity :: INFINITY | e) Unit))
type Tests = forall e. Array (Test e)
