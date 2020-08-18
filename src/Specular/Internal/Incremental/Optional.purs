module Specular.Internal.Incremental.Optional
  ( Optional
  , some
  , none
  , isSome
  , fromSome
  ) where

import Prelude

import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Reference (unsafeRefEq)

newtype Optional a = Optional a

some :: forall a. a -> Optional a
some = Optional

foreign import none :: forall a. Optional a

isSome :: forall a. Optional a -> Boolean
isSome opt = not (opt `unsafeRefEq` none)

fromSome :: forall a. Optional a -> a
fromSome opt@(Optional v)
  | isSome opt = v
  | otherwise  = unsafeCrashWith "Optional.fromSome: none"
