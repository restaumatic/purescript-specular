module Bench.Types
  ( Tests
  , Test
  ) where

import Prelude

import Data.Tuple (Tuple)
import Effect (Effect)

type Test (e :: Type) = Tuple String (Effect (Effect Unit))
type Tests = forall e. Array (Test e)
