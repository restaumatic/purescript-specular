module Specular.Internal.UniqueMap.Mutable (
    Unique
  , UniqueMap
  , new
  , insert
  , lookup
  , delete
  , values
) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

-- | A mutable associative container that generates a key on each insert.
-- The key may be later used to to look up or remove values from the map.
foreign import data UniqueMap :: Type -> Type

-- | Keys used to index UniqueMap.
foreign import data Unique :: Type

-- | Constructs an empty UniqueMap.
foreign import new :: forall a. Effect (UniqueMap a)

-- | Inserts the given value into the map under a new unique key.
foreign import insert :: forall a. a -> UniqueMap a -> Effect Unique

-- | Returns the value associated with a particular key, or Nothing if it's not there.
lookup :: forall a. Unique -> UniqueMap a -> Effect (Maybe a)
lookup key m = lookupImpl key m Just Nothing

-- | Removes the value associated with the given key from the map.
foreign import delete :: forall a. Unique -> UniqueMap a -> Effect Unit

-- | Returns all values inserted into the map so far.
foreign import values :: forall a. UniqueMap a -> Effect (Array a)

foreign import lookupImpl ::
    forall a
  . Unique
 -> UniqueMap a
 -> (a -> Maybe a) -- ^ Just
 -> Maybe a        -- ^ Nothing
 -> Effect (Maybe a)
