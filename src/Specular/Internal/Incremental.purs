module Specular.Internal.Incremental where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, mkEffectFn3, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Specular.Internal.ExclusiveTask as ExclusiveTask
import Specular.Internal.Effect (nextMicrotask)
import Specular.Internal.Incremental.Array as Array
import Specular.Internal.Incremental.Effect (foreachUntil)
import Specular.Internal.Incremental.Global (globalCurrentStabilizationNum, globalLastStabilizationNum, globalTotalRefcount, stabilizationIsNotInProgress)
import Specular.Internal.Incremental.Mutable (Field(..))
import Specular.Internal.Incremental.MutableArray as MutableArray
import Specular.Internal.Incremental.Node (Node, Observer, SomeNode, toSomeNode, toSomeNodeArray)
import Specular.Internal.Incremental.Node as Node
import Specular.Internal.Incremental.Optional (Optional)
import Specular.Internal.Incremental.Optional as Optional
import Specular.Internal.Incremental.PriorityQueue as PQ
import Specular.Internal.Incremental.Ref as Ref
import Specular.Internal.Profiling as Profiling
import Specular.Internal.Queue (Queue)
import Specular.Internal.Queue as Queue
import Unsafe.Coerce (unsafeCoerce)

type Unsubscribe = Effect Unit

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
  node <- runEffectFn1 Node.create
    { compute: mkEffectFn1 \node -> do
        value <- runEffectFn1 Node.valueExc node
        pure (Optional.some value)
    , dependencies: pure []
    }
  runEffectFn2 Node.set_value node (Optional.some val)
  pure (Var node)

setVar :: forall a. EffectFn2 (Var a) a Unit
setVar = mkEffectFn2 \(Var node) val -> do
  runEffectFn2 Node.set_value node (Optional.some val)
  runEffectFn1 addToRecomputeQueue node

addToRecomputeQueue :: forall a. EffectFn1 (Node a) Unit
addToRecomputeQueue = mkEffectFn1 \node -> do
  _ <- runEffectFn2 PQ.add globalRecomputeQueue (toSomeNode node)
  pure unit

readVar :: forall a. Var a -> Node a
readVar (Var x) = x

-- * Event

newtype Event a = Event (Node a)

newEvent :: forall a. Effect (Event a)
newEvent = do
  node <- runEffectFn1 Node.create
    { compute: mkEffectFn1 \node -> do
        runEffectFn1 Node.get_value node
    , dependencies: pure []
    }
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

addDependent :: forall a. EffectFn2 (Node a) SomeNode Unit
addDependent = mkEffectFn2 \node dependent -> do
  --  trace $ "addDependent " <> show (Node.name' node) <> " -> " <> show (Node.name' dependent)

  oldRefcount <- runEffectFn1 Node.refcount node
  dependents <- runEffectFn1 Node.get_dependents node
  runEffectFn2 MutableArray.push dependents dependent
  runEffectFn2 handleRefcountChange node oldRefcount

removeDependent :: forall a. EffectFn2 (Node a) SomeNode Unit
removeDependent = mkEffectFn2 \node dependent -> do
  oldRefcount <- runEffectFn1 Node.refcount node
  dependents <- runEffectFn1 Node.get_dependents node
  runEffectFn2 MutableArray.remove dependents dependent
  runEffectFn2 handleRefcountChange node oldRefcount

handleRefcountChange :: forall a. EffectFn2 (Node a) Int Unit
handleRefcountChange = mkEffectFn2 \node oldRefcount -> do
  newcount <- runEffectFn1 Node.refcount node
  if oldRefcount == 0 && newcount > 0 then
    runEffectFn1 connect node
  else if oldRefcount > 0 && newcount == 0 then
    runEffectFn1 disconnect node
  else
    pure unit

  -- Update globalTotalRefcount
  oldTotalRefcount <- runEffectFn1 Ref.read globalTotalRefcount
  runEffectFn2 Ref.write globalTotalRefcount (oldTotalRefcount - oldRefcount + newcount)

-- Preconditions:
-- - node does not have value computed
-- - node does not have any dependents
--
-- Postconditions:
-- - all dependencies are connected and have value computed
-- - node has value computed
-- - node has correct height
connect :: forall a. EffectFn1 (Node a) Unit
connect = mkEffectFn1 \node -> do
  mark <- runEffectFn1 Profiling.begin ("connect " <> Node.name node)

  source <- runEffectFn1 Node.get_source node
  dependencies <- source.dependencies

  runEffectFn2 Array.iterate dependencies $ mkEffectFn1 \dependency -> do
    runEffectFn2 addDependent dependency (toSomeNode node)
    dependencyHeight <- runEffectFn1 Node.get_height dependency
    adjustedHeight <- runEffectFn1 Node.get_adjustedHeight node
    ourHeight <- runEffectFn1 Node.get_height node
    let desiredHeight = max (dependencyHeight + 1) adjustedHeight
    -- trace $ "connect " <> show (Node.name node) <> " -> " <> show (Node.name dependency) <> " dependencyHeight=" <> show dependencyHeight <> " ourHeight=" <> show ourHeight
    if desiredHeight > ourHeight then do
      runEffectFn2 Node.set_height node desiredHeight
      runEffectFn2 Node.set_adjustedHeight node desiredHeight
    else
      pure unit

  value <- runEffectFn1 source.compute node

  -- Note: `compute` can return `none`, which means "do not update the value",
  -- in cases where we're re-connecting a node which was previously alive.
  -- In that case, don't erase the previous value.
  if Optional.isSome value then
    runEffectFn2 Node.set_value node value
  else
    pure unit

  runEffectFn1 Profiling.end mark

disconnect :: forall a. EffectFn1 (Node a) Unit
disconnect = mkEffectFn1 \node -> do
  mark <- runEffectFn1 Profiling.begin ("disconnect " <> Node.name node)

  source <- runEffectFn1 Node.get_source node

  dependencies <- source.dependencies
  runEffectFn2 Array.iterate dependencies $ mkEffectFn1 \dependency -> do
    runEffectFn2 removeDependent dependency (toSomeNode node)

  runEffectFn1 Profiling.end mark

-- * Effect queue

globalEffectQueue :: Queue (Effect Unit)
globalEffectQueue = unsafePerformEffect Queue.new

subscribeNode :: forall a. EffectFn2 (a -> Effect Unit) (Node a) Unsubscribe
subscribeNode = mkEffectFn2 \handler node -> do
  let
    h = mkEffectFn1 \value -> do
      runEffectFn2 Queue.enqueue globalEffectQueue (handler value)
  runEffectFn2 addObserver node h
  pure (runEffectFn2 removeObserver node h)

-- * Recompute

stabilize :: Effect Unit
stabilize = do
  mark <- runEffectFn1 Profiling.begin "stabilize"

  stabilizationNum <- runEffectFn1 Ref.read globalCurrentStabilizationNum
  if stabilizationNum /= stabilizationIsNotInProgress then
    unsafeCrashWith "Specular: stabilize called when stabilization already in progress"
  else pure unit

  oldStabilizationNum <- runEffectFn1 Ref.read globalLastStabilizationNum
  let currentStabilizationNum = oldStabilizationNum + 1
  runEffectFn2 Ref.write globalLastStabilizationNum currentStabilizationNum
  runEffectFn2 Ref.write globalCurrentStabilizationNum currentStabilizationNum

  runEffectFn2 PQ.drain globalRecomputeQueue recomputeNode

  runEffectFn2 Ref.write globalCurrentStabilizationNum stabilizationIsNotInProgress
  runEffectFn1 Profiling.end mark

  mark2 <- runEffectFn1 Profiling.begin "drainEffects"
  runEffectFn2 Queue.drain globalEffectQueue (mkEffectFn1 \handler -> handler)
  runEffectFn1 Profiling.end mark2

recomputeNode :: EffectFn1 SomeNode Unit
recomputeNode = mkEffectFn1 \node -> do
  height <- runEffectFn1 Node.get_height node
  adjustedHeight <- runEffectFn1 Node.get_adjustedHeight node

  if adjustedHeight > height then do
    mark <- runEffectFn1 Profiling.begin ("bump height " <> Node.name node)

    --    trace $ "stabilize: node " <> show name <> ": height bump " <> show height <> " -> " <> show adjustedHeight

    dependents <- runEffectFn1 Node.get_dependents node
    runEffectFn2 MutableArray.iterate dependents $ mkEffectFn1 \dependent -> do
      runEffectFn2 ensureHeight dependent (adjustedHeight + 1)

    runEffectFn2 Node.set_height node adjustedHeight

    -- Reconsider the node with new height
    _ <- runEffectFn2 PQ.add globalRecomputeQueue node

    runEffectFn1 Profiling.end mark

  else do
    mark <- runEffectFn1 Profiling.begin ("compute " <> Node.name node)
    --    trace $ "stabilize: node " <> show name <> ": compute at height " <> show height

    source <- runEffectFn1 Node.get_source node
    -- oldValue_opt <- runEffectFn1 Node.get_value node
    newValue_opt <- runEffectFn1 source.compute node

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
  runEffectFn1 Node.create
    { compute: mkEffectFn1 \_ -> pure (Optional.some value)
    , dependencies: pure []
    }

map :: forall a b. EffectFn2 (a -> b) (Node a) (Node b)
map = mkEffectFn2 \fn a -> do
  let deps = [ toSomeNode a ]
  runEffectFn1 Node.create
    { compute: mkEffectFn1 \_ -> do
        value_a <- runEffectFn1 Node.valueExc a
        pure (Optional.some (fn value_a))
    , dependencies: pure deps
    }

mapOptional :: forall a b. EffectFn2 (a -> Optional b) (Node a) (Node b)
mapOptional = mkEffectFn2 \fn a -> do
  let deps = [ toSomeNode a ]
  runEffectFn1 Node.create
    { compute: mkEffectFn1 \_ -> do
        value_a <- runEffectFn1 Node.get_value a
        pure
          ( if Optional.isSome value_a then fn (Optional.fromSome value_a)
            else Optional.none
          )
    , dependencies: pure deps
    }

data AsyncComputation a = Sync a | Async (Aff a)

data AsyncState a
  = InProgress (Maybe (Either Error a))
  | Finished (Either Error a)

derive instance Generic (AsyncState a) _
instance Show a => Show (AsyncState a) where
  show = genericShow

mapAsync :: forall a b. EffectFn2 (a -> AsyncComputation b) (Node a) (Node (AsyncState b))
mapAsync = mkEffectFn2 \fn a -> do
  let deps = [ toSomeNode a ]
  task <- ExclusiveTask.new
  finishedRef <- runEffectFn1 Ref.new Nothing
  runEffectFn1 Node.create
    { compute: mkEffectFn1 \self -> do
        -- Need to determine why we're updating - because the input changed, or because the computation finished?
        finished <- runEffectFn1 Ref.read finishedRef
        case finished of
          Nothing -> do
            value_a <- runEffectFn1 Node.valueExc a
            case fn value_a of
              Sync x ->
                pure (Optional.some (Finished (Right x)))
              Async aff -> do
                nextMicrotask do
                  ExclusiveTask.run task do
                    newValue <- aff
                    liftEffect do
                      runEffectFn2 Ref.write finishedRef (Just (Right newValue))
                      runEffectFn1 addToRecomputeQueue self
                      stabilize
                previous <- runEffectFn1 Node.get_value self
                pure (Optional.some (InProgress (getPreviousValue previous)))
          Just x -> do
            -- Hmm. Can the input also be changing at the same time we're reporting the result of async computation?
            -- Currently not, because we always stabilize after changing a node.
            -- But if we introduced some batching later on, it could happen,
            -- in which case we'd need to check `isChangingInCurrentStabilization` of our dependency.

            runEffectFn2 Ref.write finishedRef Nothing
            pure (Optional.some (Finished x))
    , dependencies: pure deps
    }

  where
  getPreviousValue opt
    | Optional.isSome opt =
      case Optional.fromSome opt of
        InProgress x -> x
        Finished x -> Just x
    | otherwise = Nothing

map2 :: forall a b c. EffectFn3 (Fn2 a b c) (Node a) (Node b) (Node c)
map2 = mkEffectFn3 \fn a b -> do
  let deps = [ toSomeNode a, toSomeNode b ]
  runEffectFn1 Node.create
    { compute: mkEffectFn1 \_ -> do
        value_a <- runEffectFn1 Node.valueExc a
        value_b <- runEffectFn1 Node.valueExc b
        pure (Optional.some (runFn2 fn value_a value_b))
    , dependencies: pure deps
    }

mapN :: forall a b. EffectFn2 (Array a -> b) (Array (Node a)) (Node b)
mapN = mkEffectFn2 \fn inputs -> do
  runEffectFn1 Node.create
    { compute: mkEffectFn1 \_ -> do
        values <- runEffectFn2 Array.mapE inputs Node.valueExc
        pure (Optional.some (fn values))
    , dependencies: pure (toSomeNodeArray inputs)
    }

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
  main_node_ref <- runEffectFn1 Ref.new Optional.none
  rhs_node <- runEffectFn1 Node.create
    { compute: mkEffectFn1 \node -> do

        value_lhs <- runEffectFn1 Node.valueExc lhs
        let rhs = fn value_lhs

        -- adjust dependencies and height of the main node, taking new dependency into account
        main_opt <- runEffectFn1 Ref.read main_node_ref
        let main = Optional.fromSome main_opt

        runEffectFn2 addDependent rhs (toSomeNode main)
        dependencyHeight <- runEffectFn1 Node.get_height rhs
        runEffectFn2 ensureHeight main (dependencyHeight + 1)

        -- disconnect from old dependency, if present
        old_rhs_opt <- runEffectFn1 Node.get_value node
        if Optional.isSome old_rhs_opt then do
          runEffectFn2 removeDependent (Optional.fromSome old_rhs_opt) (toSomeNode main)
        else pure unit

        pure (Optional.some rhs)
    , dependencies: do
        pure [ toSomeNode lhs ]
    }
  runEffectFn2 Node.annotate rhs_node "switch data"
  main <- runEffectFn1 Node.create
    { compute: mkEffectFn1 \_ -> do
        rhs <- runEffectFn1 Node.valueExc rhs_node
        isFiring <- runEffectFn1 Node.isChangingInCurrentStabilization rhs
        if alwaysFire || isFiring then do
          runEffectFn1 Node.get_value rhs
        else
          pure Optional.none
    , dependencies: do
        rhs_opt <- runEffectFn1 Node.get_value rhs_node
        if Optional.isSome rhs_opt then
          pure [ toSomeNode rhs_node, toSomeNode (Optional.fromSome rhs_opt) ]
        else
          -- if we don't know the rhs yet, `compute` in rhs proxy will add it
          pure [ toSomeNode rhs_node ]
    }
  runEffectFn2 Ref.write main_node_ref (Optional.some main)
  pure main

fold :: forall a b. EffectFn3 (Fn2 a b (Optional b)) b (Node a) (Node b)

fold = mkEffectFn3 \fn initial a -> do
  let deps = [ toSomeNode a ]
  runEffectFn1 Node.create
    { compute: mkEffectFn1 \node -> do
        state_opt <- runEffectFn1 Node.get_value node
        let state = if Optional.isSome state_opt then Optional.fromSome state_opt else initial

        hasInput <- runEffectFn1 Node.isChangingInCurrentStabilization a

        result <-
          if hasInput then do
            input <- runEffectFn1 Node.valueExc a
            pure (runFn2 fn input state)
          else
            pure (Optional.some state)

        --        trace $ "fold: " <> (unsafeCoerce result :: String) <> ", " <> (unsafeCoerce result).constructor.name
        pure result
    , dependencies: pure deps
    }

uniqBy :: forall a. EffectFn2 (Fn2 a a Boolean) (Node a) (Node a)
uniqBy = mkEffectFn2 \fn a -> do
  let deps = [ toSomeNode a ]
  runEffectFn1 Node.create
    { compute: mkEffectFn1 \node -> do
        old <- runEffectFn1 Node.get_value node
        new <- runEffectFn1 Node.get_value a

        if Optional.isSome new && (not (Optional.isSome old) || not (runFn2 fn (Optional.fromSome old) (Optional.fromSome new))) then do
          pure new
        else
          pure Optional.none

    , dependencies: pure deps
    }

sample :: forall a b c. EffectFn3 (Fn2 a b (Optional c)) (Node a) (Node b) (Node c)
sample = mkEffectFn3 \fn signal clock -> do
  runEffectFn1 Node.create
    { compute: mkEffectFn1 \_node -> do
        hasInput <- runEffectFn1 Node.isChangingInCurrentStabilization clock

        result <-
          if hasInput then do
            signal_value <- runEffectFn1 Node.valueExc signal
            clock_value <- runEffectFn1 Node.valueExc clock
            pure (runFn2 fn signal_value clock_value)
          else
            pure Optional.none

        pure result
    , dependencies: pure [ toSomeNode signal, toSomeNode clock ]
    }

leftmost :: forall a. EffectFn1 (Array (Node a)) (Node a)
leftmost = mkEffectFn1 \inputs -> do
  runEffectFn1 Node.create
    { compute: mkEffectFn1 \_node -> do
        runEffectFn2 foreachUntil inputs $ mkEffectFn1 \input -> do
          isFiring <- runEffectFn1 Node.isChangingInCurrentStabilization input
          if isFiring then
            runEffectFn1 Node.get_value input
          else
            pure Optional.none
    , dependencies: pure (toSomeNodeArray inputs)
    }

traceChanges :: forall a. EffectFn2 (EffectFn1 a Unit) (Node a) (Node a)
traceChanges = mkEffectFn2 \fn input -> do
  runEffectFn1 Node.create
    { compute: mkEffectFn1 \_node -> do
        value_opt <- runEffectFn1 Node.get_value input
        isFiring <- runEffectFn1 Node.isChangingInCurrentStabilization input
        if isFiring then runEffectFn1 fn (Optional.fromSome value_opt) else pure unit
        pure value_opt
    , dependencies: pure [ toSomeNode input ]
    }

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
