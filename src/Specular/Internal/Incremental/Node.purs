module Specular.Internal.Incremental.Node
  ( module Specular.Internal.Incremental.Node
  , module Specular.Internal.Incremental.Mutable
  ) where

import Prelude

import Effect (Effect)
import Specular.Internal.Incremental.Ref as Ref
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn6, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn6)
import Effect.Unsafe (unsafePerformEffect)
import Specular.Internal.Incremental.Global (globalCurrentStabilizationNum)
import Specular.Internal.Incremental.MutableArray (MutableArray)
import Specular.Internal.Incremental.MutableArray as MutableArray
import Specular.Internal.Incremental.Mutable (Any, Field(..), Immutable, Mutable)
import Specular.Internal.Incremental.Optional (Optional)
import Specular.Internal.Incremental.Optional as Optional
import Specular.Internal.Profiling as Profiling
import Unsafe.Coerce (unsafeCoerce)

foreign import data Node :: Type -> Type

type Observer a = EffectFn1 a Unit

-- * Node fields

-- [[[cog
-- immutable_fields = [
--   ['dependents', 'MutableArray SomeNode'],
--   ['observers', 'MutableArray (Observer a)'],
--   ['source', 'Source a'],
-- ]
-- mutable_fields = [
--   ['adjustedHeight', 'Int'],
--   ['changedAt', 'Int'],
--   ['height', 'Int'],
--   ['name', 'String'],
--   ['value', 'Optional a'],
-- ]
-- for name, ty in immutable_fields:
--     cog.outl("""
--     foreign import get_%(name)s :: forall a. EffectFn1 (Node a) (%(ty)s)
--     """ % {'name': name, 'ty': ty})
-- for name, ty in mutable_fields:
--     cog.outl("""
--     foreign import get_%(name)s :: forall a. EffectFn1 (Node a) (%(ty)s)
--     foreign import set_%(name)s :: forall a. EffectFn2 (Node a) (%(ty)s) Unit
--     """ % {'name': name, 'ty': ty})
-- ]]]

foreign import get_dependents :: forall a. EffectFn1 (Node a) (MutableArray SomeNode)


foreign import get_observers :: forall a. EffectFn1 (Node a) (MutableArray (Observer a))


foreign import get_source :: forall a. EffectFn1 (Node a) (Source a)


foreign import get_adjustedHeight :: forall a. EffectFn1 (Node a) (Int)
foreign import set_adjustedHeight :: forall a. EffectFn2 (Node a) (Int) Unit


foreign import get_changedAt :: forall a. EffectFn1 (Node a) (Int)
foreign import set_changedAt :: forall a. EffectFn2 (Node a) (Int) Unit


foreign import get_height :: forall a. EffectFn1 (Node a) (Int)
foreign import set_height :: forall a. EffectFn2 (Node a) (Int) Unit


foreign import get_name :: forall a. EffectFn1 (Node a) (String)
foreign import set_name :: forall a. EffectFn2 (Node a) (String) Unit


foreign import get_value :: forall a. EffectFn1 (Node a) (Optional a)
foreign import set_value :: forall a. EffectFn2 (Node a) (Optional a) Unit

-- [[[end]]]

foreign import _new ::
  forall a.
  EffectFn6
    (Optional Any) -- Optional.none
    (Source a)
    (MutableArray SomeNode)
    (MutableArray (Observer a))
    (Optional a)
    Int
  (Node a)

-- * Node source

type Source a =
  { compute :: EffectFn1 (Node a) (Optional a)
    -- Compute the node value, if none is returned then we don't change value and don't propagate.
  , dependencies :: Effect (Array SomeNode)
  }

-- * Existential

type SomeNode = Node Any

toSomeNode :: forall a. Node a -> SomeNode
toSomeNode = unsafeCoerce

toSomeNodeArray :: forall a. Array (Node a) -> Array SomeNode
toSomeNodeArray = unsafeCoerce

-- * Creation

create :: forall a. EffectFn1 (Source a) (Node a)
create = mkEffectFn1 \source -> do
  dependents <- MutableArray.empty
  observers <- MutableArray.empty
  runEffectFn6 _new
    Optional.none
    source
    dependents
    observers
    Optional.none -- value
    0             -- height

-- * Utils

refcount :: forall a. EffectFn1 (Node a) Int
refcount = mkEffectFn1 \node -> do
  observers <- runEffectFn1 get_observers node
  numDependents <- runEffectFn1 MutableArray.length observers
  dependents <- runEffectFn1 get_dependents node
  numObservers <- runEffectFn1 MutableArray.length dependents
  pure (numDependents + numObservers)

valueExc :: forall a. EffectFn1 (Node a) a
valueExc = mkEffectFn1 \node -> do
  value_opt <- runEffectFn1 get_value node
  pure (Optional.fromSome value_opt)

annotate :: forall a. EffectFn2 (Node a) String Unit
annotate = if Profiling.enabled then set_name else mkEffectFn2 \_ _ -> pure unit

name :: forall a. Node a -> String
name node = unsafePerformEffect (runEffectFn1 get_name node)

isChangingInCurrentStabilization :: forall a. EffectFn1 (Node a) Boolean
isChangingInCurrentStabilization = mkEffectFn1 \node -> do
  currentStabilizationNum <- runEffectFn1 Ref.read globalCurrentStabilizationNum
  changedAt <- runEffectFn1 get_changedAt node
  pure (changedAt == currentStabilizationNum)
