module Specular.Internal.Incremental.Mutable where

data Mutability

foreign import data Mutable :: Mutability
foreign import data Immutable :: Mutability

newtype Field :: forall k1 k2. k1 -> Mutability -> k2 -> Type
newtype Field s (m :: Mutability) a = Field String

foreign import data Any :: Type
