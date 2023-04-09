module Specular.Internal.Incremental where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, mkEffectFn3, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Specular.Internal.Incremental.Array as Array
import Specular.Internal.Incremental.Effect (foreachUntil)
import Specular.Internal.Incremental.Global (globalCurrentStabilizationNum, globalTotalRefcount, globalLastStabilizationNum, stabilizationIsNotInProgress)
import Specular.Internal.Incremental.Mutable (Field(..))
import Specular.Internal.Incremental.MutableArray as Mutability
import Specular.Internal.Incremental.MutableArray as MutableArray
import Specular.Internal.Incremental.Node (ComputeFn, Node, Observer, SomeNode, toSomeNode, toSomeNodeArray)
import Specular.Internal.Incremental.Node as Node
import Specular.Internal.Incremental.Optional (Optional)
import Specular.Internal.Incremental.Optional as Optional
import Specular.Internal.Incremental.PriorityQueue as PQ
import Specular.Internal.Incremental.Ref as Ref
import Specular.Internal.Profiling as Profiling
import Unsafe.Coerce (unsafeCoerce)

-- | Priority queue for propagating node changes in dependency order.
globalRecomputeQueue :: PQ.PQ SomeNode
globalRecomputeQueue = unsafePerformEffect $
  runEffectFn4 PQ.new
    Optional.none
    (Field "height")
    (Field "inRecomputeQueue")
    (Field "nextInRecomputeQueue")

-- * Var

newtype Var a = Var (Node a)

newVar :: forall a. EffectFn1 a (Var a)
newVar = mkEffectFn1 \val -> do
  node <- runEffectFn4 Node.create Optional.none [] compute_root unit
  runEffectFn2 Node.set_value node (Optional.some val)
  pure (Var node)

compute_root :: forall a. ComputeFn Unit a
compute_root = mkEffectFn2 \_ node -> do
  runEffectFn1 Node.get_value node

setVar :: forall a. EffectFn2 (Var a) a Unit
setVar = mkEffectFn2 \(Var node) val -> do
  runEffectFn2 Node.set_value node (Optional.some val)
  _ <- runEffectFn2 PQ.add globalRecomputeQueue (toSomeNode node)
  pure unit

readVar :: forall a. Var a -> Node a
readVar (Var x) = x

-- * Event

newtype Event a = Event (Node a)

newEvent :: forall a. Effect (Event a)
newEvent = do
  node <- runEffectFn4 Node.create
    Optional.none
    []
    compute_root
    unit
  pure (Event node)

triggerEvent :: forall a. EffectFn2 (Event a) a Unit
triggerEvent = mkEffectFn2 \(Event node) val -> do
  runEffectFn2 Node.set_value node (Optional.some val)
  _ <- runEffectFn2 PQ.add globalRecomputeQueue (toSomeNode node)
  pure unit

readEvent :: forall a. Event a -> Node a
readEvent (Event x) = x

-- * Observers and dependents

addObserver :: forall a. EffectFn2 (Node a) (Observer a) Unit
addObserver = mkEffectFn2 \node observer -> do
  oldRefcount <- runEffectFn1 Node.refcount node
  observers <- runEffectFn1 Node.get_observers node
  runEffectFn2 MutableArray.push observers observer
  runEffectFn2 handleRefcountChange node oldRefcount

removeObserver :: forall a. EffectFn2 (Node a) (Observer a) Unit
removeObserver = mkEffectFn2 \node observer -> do
  oldRefcount <- runEffectFn1 Node.refcount node
  observers <- runEffectFn1 Node.get_observers node
  runEffectFn2 MutableArray.remove observers observer
  runEffectFn2 handleRefcountChange node oldRefcount

foreign import addDependent :: forall a. EffectFn2 (Node a) SomeNode Unit
foreign import removeDependent :: forall a. EffectFn2 (Node a) SomeNode Unit
foreign import handleRefcountChange :: forall a. EffectFn2 (Node a) Int Unit

-- Preconditions:
-- - node does not have value computed
-- - node does not have any dependents
--
-- Postconditions:
-- - all dependencies are connected and have value computed
-- - node has value computed
-- - node has correct height
foreign import connect :: forall a. EffectFn1 (Node a) Unit
foreign import disconnect :: forall a. EffectFn1 (Node a) Unit

-- * Recompute

stabilize :: Effect Unit
stabilize = do
  mark <- runEffectFn1 Profiling.begin "stabilize"

  oldStabilizationNum <- runEffectFn1 Ref.read globalLastStabilizationNum
  let currentStabilizationNum = oldStabilizationNum + 1
  runEffectFn2 Ref.write globalLastStabilizationNum currentStabilizationNum
  runEffectFn2 Ref.write globalCurrentStabilizationNum currentStabilizationNum

  runEffectFn2 PQ.drain globalRecomputeQueue recomputeNode

  runEffectFn2 Ref.write globalCurrentStabilizationNum stabilizationIsNotInProgress
  runEffectFn1 Profiling.end mark

recomputeNode :: EffectFn1 SomeNode Unit
recomputeNode = mkEffectFn1 \node -> do
  height <- runEffectFn1 Node.get_height node
  adjustedHeight <- runEffectFn1 Node.get_adjustedHeight node

  if adjustedHeight > height then do
    mark <- runEffectFn1 Profiling.begin ("bump height " <> Node.name node)

    -- trace $ "stabilize: node " <> Node.name node <> ": height bump " <> show height <> " -> " <> show adjustedHeight

    dependents <- runEffectFn1 Node.get_dependents node
    runEffectFn2 MutableArray.iterate dependents $ mkEffectFn1 \dependent -> do
      runEffectFn2 ensureHeight dependent (adjustedHeight + 1)

    runEffectFn2 Node.set_height node adjustedHeight

    -- Reconsider the node with new height
    _ <- runEffectFn2 PQ.add globalRecomputeQueue node

    runEffectFn1 Profiling.end mark

  else do
    mark <- runEffectFn1 Profiling.begin ("compute " <> Node.name node)
    -- trace $ "stabilize: node " <> Node.name node <> ": compute at height " <> show height

    -- oldValue_opt <- runEffectFn1 Node.get_value node
    newValue_opt <- runEffectFn1 Node.compute node

    if Optional.isSome newValue_opt
    -- && shouldNotCutOff oldValue_opt newValue
    then do
      let newValue = Optional.fromSome newValue_opt
      runEffectFn2 Node.set_value node (Optional.some newValue)
      currentStabilizationNum <- runEffectFn1 Ref.read globalCurrentStabilizationNum
      runEffectFn2 Node.set_changedAt node currentStabilizationNum

      dependents <- runEffectFn1 Node.get_dependents node
      runEffectFn2 MutableArray.iterate dependents $ mkEffectFn1 \dependent -> do
        _added <- runEffectFn2 PQ.add globalRecomputeQueue dependent
        --        if added then do
        --          dependentName <- runEffectFn1 Node.name dependent
        --          trace $ "stabilize: node " <> show dependentName <> " added to recompute queue"
        --        else do
        --          dependentName <- runEffectFn1 Node.name dependent
        --          trace $ "stabilize: node " <> show dependentName <> " already in recompute queue"
        pure unit

      observers <- runEffectFn1 Node.get_observers node
      runEffectFn2 MutableArray.iterate observers $ mkEffectFn1 \observer -> do
        -- FIXME: should be done outside stabilize loop, to avoid interfering with the process
        -- (like in Specular - a FIFO queue)
        runEffectFn1 observer newValue
    else do
      --      trace $ "stabilize: node " <> show name <> " cut off"
      pure unit

    runEffectFn1 Profiling.end mark

-- * Computational nodes

constant :: forall a. EffectFn1 a (Node a)
constant = mkEffectFn1 \value -> do
  -- Same as Var, actually
  node <- runEffectFn4 Node.create Optional.none [] compute_root unit
  runEffectFn2 Node.set_value node (Optional.some value)
  pure node

map :: forall a b. EffectFn2 (a -> b) (Node a) (Node b)
map = mkEffectFn2 \fn a -> do
  runEffectFn4 Node.create
    Optional.none
    [ toSomeNode a ]
    compute_map
    fn

compute_map :: forall a b. ComputeFn (a -> b) b
compute_map = mkEffectFn2 \fn node -> do
  (input :: Node a) <- runEffectFn2 Node.get_dependency node 0
  value_a <- runEffectFn1 Node.valueExc input
  pure (Optional.some (fn value_a))


mapOptional :: forall a b. EffectFn2 (a -> Optional b) (Node a) (Node b)
mapOptional = mkEffectFn2 \fn a -> do
  runEffectFn4 Node.create
    Optional.none
    [ toSomeNode a ]
    compute_mapOptional
    fn

compute_mapOptional :: forall a b. ComputeFn (a -> Optional b) b
compute_mapOptional = mkEffectFn2 \fn node -> do
  (input :: Node a) <- runEffectFn2 Node.get_dependency node 0
  value_a <- runEffectFn1 Node.get_value input
  pure
    ( if Optional.isSome value_a then fn (Optional.fromSome value_a)
      else Optional.none
    )

map2 :: forall a b c. EffectFn3 (Fn2 a b c) (Node a) (Node b) (Node c)
map2 = mkEffectFn3 \fn a b ->
  runEffectFn4 Node.create (Optional.none) [ toSomeNode a, toSomeNode b ] compute_map2 fn

compute_map2 :: forall a b c. ComputeFn (Fn2 a b c) c
compute_map2 = mkEffectFn2 \fn node -> do
  (a :: Node a) <- runEffectFn2 Node.get_dependency node 0
  (b :: Node b) <- runEffectFn2 Node.get_dependency node 1
  value_a <- runEffectFn1 Node.valueExc a
  value_b <- runEffectFn1 Node.valueExc b
  pure (Optional.some (runFn2 fn value_a value_b))

mapN :: forall a b. EffectFn2 (Array a -> b) (Array (Node a)) (Node b)
mapN = mkEffectFn2 \fn inputs -> do
  runEffectFn4 Node.create
    Optional.none
    (toSomeNodeArray inputs)
    compute_mapN
    fn

compute_mapN :: forall a b. ComputeFn (Array a -> b) b
compute_mapN = mkEffectFn2 \fn node -> do
  deps <- runEffectFn1 Node.get_dependencies node
  values <- runEffectFn2 Array.mapE ((unsafeCoerce :: MutableArray.MutableArray SomeNode -> Array (Node a)) deps) Node.valueExc
  pure (Optional.some (fn values))


-- Problem with bind and connect:
-- We have to:
-- - first compute LHS
-- - then compute ourselves
-- - only then we know the second dependency
-- - compute the second dependency
--
-- Problem with bind and compute:
-- - if we compute and the LHS changes, our height changes! We may need to re-add ourselves to recompute queue
bind_ :: forall a b. EffectFn2 (Node a) (a -> Node b) (Node b)
bind_ = mkEffectFn2 \lhs fn -> do
  runEffectFn3 switch true lhs fn

switch :: forall a b. EffectFn3 Boolean (Node a) (a -> Node b) (Node b)
switch = mkEffectFn3 \alwaysFire lhs fn -> do
  mainNodeRef <- runEffectFn1 Ref.new Optional.none
  let state = { lhs, mainNodeRef, fn, alwaysFire }
  rhs_node <- runEffectFn4 Node.create Optional.none [ toSomeNode lhs ] compute_switch_rhs state
  runEffectFn2 Node.annotate rhs_node "switch_rhs"
  main <- runEffectFn4 Node.create Optional.none [ toSomeNode rhs_node ] compute_switch_main state
  runEffectFn2 Ref.write mainNodeRef (Optional.some main)
  pure main

type SwitchState a b =
  { lhs :: Node a
  , mainNodeRef :: Ref (Optional (Node b))
  , fn :: a -> Node b
  , alwaysFire :: Boolean
  }

compute_switch_rhs :: forall a b. ComputeFn (SwitchState a b) (Node b)
compute_switch_rhs = mkEffectFn2 \state node -> do
  value_lhs <- runEffectFn1 Node.valueExc state.lhs
  let rhs = state.fn value_lhs

  -- adjust dependencies and height of the main node, taking new dependency into account
  main_opt <- runEffectFn1 Ref.read state.mainNodeRef
  let main = Optional.fromSome main_opt

  old_rhs_opt <- runEffectFn1 Node.get_value node

  -- trace "add dep in switch_rhs"
  runEffectFn2 addDependent rhs (toSomeNode main)
  dependencyHeight <- runEffectFn1 Node.get_height rhs
  runEffectFn2 ensureHeight main (dependencyHeight + 1)

  -- disconnect from old dependency, if present
  if Optional.isSome old_rhs_opt then do
    runEffectFn2 removeDependent (Optional.fromSome old_rhs_opt) (toSomeNode main)
  else pure unit

  -- trace $ "computed " <> Node.name node <> " old_rhs="
  --   <> (if Optional.isSome old_rhs_opt then Node.name (Optional.fromSome old_rhs_opt) else "(none)") <> " new_rhs=" <> Node.name rhs

  pure (Optional.some rhs)


compute_switch_main :: forall a b. ComputeFn (SwitchState a b) b
compute_switch_main = mkEffectFn2 \state node -> do
  rhs_node <- runEffectFn2 Node.get_dependency node 0
  -- trace $ "rhs_node=" <> Node.name rhs_node
  rhs <- runEffectFn1 Node.valueExc rhs_node

  deps <- runEffectFn1 Node.get_dependencies node
  runEffectFn3 MutableArray.write deps 1 (toSomeNode rhs)

  isFiring <- runEffectFn1 Node.isChangingInCurrentStabilization rhs
  if state.alwaysFire || isFiring then do
    runEffectFn1 Node.get_value rhs
  else
    pure Optional.none

fold :: forall a b. EffectFn3 (Fn2 a b (Optional b)) b (Node a) (Node b)
fold = mkEffectFn3 \fn initial a -> do
  node <- runEffectFn4 Node.create Optional.none [ toSomeNode a ] compute_fold fn
  runEffectFn2 Node.set_value node (Optional.some initial)
  pure node

compute_fold :: forall a b. ComputeFn (Fn2 a b (Optional b)) b
compute_fold = mkEffectFn2 \fn node -> do
  (a :: Node a) <- runEffectFn2 Node.get_dependency node 0
  state_opt <- runEffectFn1 Node.get_value node
  let state = Optional.fromSome state_opt

  hasInput <- runEffectFn1 Node.isChangingInCurrentStabilization a

  result <-
    if hasInput then do
      input <- runEffectFn1 Node.valueExc a
      pure (runFn2 fn input state)
    else
      pure (Optional.some state)

  --        trace $ "fold: " <> (unsafeCoerce result :: String) <> ", " <> (unsafeCoerce result).constructor.name
  pure result

sample :: forall a b c. EffectFn3 (Fn2 a b (Optional c)) (Node a) (Node b) (Node c)
sample = mkEffectFn3 \fn signal clock -> do
  runEffectFn4 Node.create
    Optional.none
    [ toSomeNode signal, toSomeNode clock ]
    compute_sample
    fn

compute_sample :: forall a b c. ComputeFn (Fn2 a b (Optional c)) c
compute_sample = mkEffectFn2 \fn node -> do
  (signal :: Node a) <- runEffectFn2 Node.get_dependency node 0
  (clock :: Node b) <- runEffectFn2 Node.get_dependency node 1
  hasInput <- runEffectFn1 Node.isChangingInCurrentStabilization clock

  result <-
    if hasInput then do
      signal_value <- runEffectFn1 Node.valueExc signal
      clock_value <- runEffectFn1 Node.valueExc clock
      pure (runFn2 fn signal_value clock_value)
    else
      pure Optional.none

  pure result

leftmost :: forall a. EffectFn1 (Array (Node a)) (Node a)
leftmost = mkEffectFn1 \inputs -> do
  runEffectFn4 Node.create Optional.none (toSomeNodeArray inputs) compute_leftmost unit

compute_leftmost :: forall a. ComputeFn Unit a
compute_leftmost = mkEffectFn2 \_ node -> do
  inputs <- runEffectFn1 Node.get_dependencies node
  runEffectFn2 foreachUntil ((unsafeCoerce :: MutableArray.MutableArray SomeNode -> Array (Node a)) inputs) $ mkEffectFn1 \input -> do
    isFiring <- runEffectFn1 Node.isChangingInCurrentStabilization input
    if isFiring then
      runEffectFn1 Node.get_value input
    else
      pure Optional.none

traceChanges :: forall a. EffectFn2 (EffectFn1 a Unit) (Node a) (Node a)
traceChanges = mkEffectFn2 \fn input -> do
  runEffectFn4 Node.create Optional.none [toSomeNode input] compute_traceChanges fn

compute_traceChanges :: forall a. ComputeFn (EffectFn1 a Unit) a
compute_traceChanges = mkEffectFn2 \fn node -> do
  input <- runEffectFn2 Node.get_dependency node 0
  value_opt <- runEffectFn1 Node.get_value input
  isFiring <- runEffectFn1 Node.isChangingInCurrentStabilization input
  if isFiring then runEffectFn1 fn (Optional.fromSome value_opt) else pure unit
  pure value_opt

-- * Adjust height

ensureHeight :: forall a. EffectFn2 (Node a) Int Unit
ensureHeight = mkEffectFn2 \node newHeight -> do
  oldAdjustedHeight <- runEffectFn1 Node.get_adjustedHeight node
  runEffectFn2 Node.set_adjustedHeight node (max oldAdjustedHeight newHeight)

-- * Utils

effectCrash :: forall a. String -> Effect a
effectCrash msg = unsafeCoerce ((\_ -> unsafeCrashWith msg) :: Unit -> a) :: Effect a

isTracing :: Boolean
isTracing = false

trace :: String -> Effect Unit
trace = if isTracing then Console.log else \_ -> pure unit
