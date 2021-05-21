module Specular.FRP.List
  ( dynamicListWithIndex
  , dynamicList
  , dynamicListWithIndex_
  , dynamicList_
  , weakDynamicListWithIndex
  , weakDynamicList
  , weakDynamicListWithIndex_
  , weakDynamicList_
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Control.Monad.Replace (class MonadReplace, Slot, newSlot, appendSlot, replaceSlot, destroySlot)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Specular.FRP (Dynamic, holdDyn, subscribeDyn_)
import Specular.FRP.Base (class MonadFRP, holdUniqDynBy, newEvent)
import Specular.FRP.WeakDynamic (WeakDynamic, holdWeakDyn, subscribeWeakDyn_, weaken)
import Specular.Internal.Effect (Ref, newRef, readRef, writeRef)
import Unsafe.Reference (unsafeRefEq)

-- | `dynamicListWithIndex dynArray handler`
-- | Render a list of items from `dynArray`. Each item will be rendered by `handler`.
-- |
-- | When the array changes, indexes that exist in both old and new array are _updated_,
-- | e.g. the Dynamics passed to handlers are changed. As an optimization,
-- | when old and new item values are the same JS object (using `===`), the
-- | Dynamic is not updated.
-- |
-- | If the array grows (a new index appears), a new handler is invoked.
-- |
-- | The resulting Dynamic represents return values from all the handlers.
dynamicListWithIndex :: forall m a b
   . MonadFRP m
  => MonadReplace m
  => Dynamic (Array a)
  -> (Int -> Dynamic a -> m b)
  -> m (Dynamic (Array b))
dynamicListWithIndex dynArray handler = do
  (latestRef :: Ref (Array (ListEntry m a b)))
    <- liftEffect $ newRef []

  mainSlot <- newSlot
  resultChanged <- newEvent
  result <- holdDyn [] resultChanged.event

  subscribeDyn_ (updateList latestRef mainSlot handler >=> resultChanged.fire) dynArray
  pure result

-- | Like `listWithIndex`, but operates on `WeakDynamics`.
weakDynamicListWithIndex :: forall m a b
   . MonadFRP m
  => MonadReplace m
  => WeakDynamic (Array a)
  -> (Int -> WeakDynamic a -> m b)
  -> m (WeakDynamic (Array b))
weakDynamicListWithIndex dynArray handler = do
  (latestRef :: Ref (Array (ListEntry m a b)))
    <- liftEffect $ newRef []

  mainSlot <- newSlot
  resultChanged <- newEvent

  let handler' i dyn = handler i (weaken dyn)
  subscribeWeakDyn_ (updateList latestRef mainSlot handler' >=> resultChanged.fire) dynArray

  holdWeakDyn resultChanged.event

type ListEntry m a b =
  { slot :: Slot m
  , fire :: a -> Effect Unit
  , result :: b }
  
updateList
  :: forall m a b
   . MonadFRP m
  => Ref (Array (ListEntry m a b)) -- ^ list entries
  -> Slot m                        -- ^ slot to add new entries
  -> (Int -> Dynamic a -> m b)     -- ^ item handler
  -> Array a                       -- ^ new array value
  -> Effect (Array b)              -- ^ new resulting array is returned
updateList latestRef mainSlot handler newArray = do
  latest <- readRef latestRef
  newEntries <- map Array.concat $ flip traverse (Array.range 0 (max (Array.length newArray) (Array.length latest))) $ \i -> do
    case Array.index latest i, Array.index newArray i of
      Just entry, Just x  -> do
        nextMicrotask $ entry.fire x
        pure []
      Just entry, Nothing -> do
        destroySlot entry.slot
        pure []
      Nothing,     Just x  -> do
        slot <- appendSlot mainSlot
        {event, fire} <- newEvent
        result <- replaceSlot slot do
          dyn <- holdUniqDynBy unsafeRefEq x event
          handler i dyn
        pure [{slot, fire, result}]
      Nothing,     Nothing ->
        pure []
  let newLatest = Array.take (Array.length newArray) $ latest <> newEntries
  writeRef latestRef newLatest
  pure $ map _.result newLatest

foreign import nextMicrotask :: Effect Unit -> Effect Unit

dynamicList :: forall m a b
   . MonadFRP m
  => MonadReplace m
  => Dynamic (Array a)
  -> (Dynamic a -> m b)
  -> m (Dynamic (Array b))
dynamicList dynArray handler = dynamicListWithIndex dynArray (\_ -> handler)

weakDynamicList :: forall m a b
   . MonadFRP m
  => MonadReplace m
  => WeakDynamic (Array a)
  -> (WeakDynamic a -> m b)
  -> m (WeakDynamic (Array b))
weakDynamicList dynArray handler = weakDynamicListWithIndex dynArray (\_ -> handler)

weakDynamicListWithIndex_ :: forall m a
   . MonadFRP m
  => MonadReplace m
  => WeakDynamic (Array a)
  -> (Int -> WeakDynamic a -> m Unit)
  -> m Unit
weakDynamicListWithIndex_ dynArray handler = void $ weakDynamicListWithIndex dynArray handler

weakDynamicList_ :: forall m a
   . MonadFRP m
  => MonadReplace m
  => WeakDynamic (Array a)
  -> (WeakDynamic a -> m Unit)
  -> m Unit
weakDynamicList_ dynArray handler = void $ weakDynamicListWithIndex dynArray (\_ -> handler)

dynamicListWithIndex_ :: forall m a
   . MonadFRP m
  => MonadReplace m
  => Dynamic (Array a)
  -> (Int -> Dynamic a -> m Unit)
  -> m Unit
dynamicListWithIndex_ dynArray handler = do
  (latestRef :: Ref (Array (ListEntry m a Unit)))
    <- liftEffect $ newRef []

  mainSlot <- newSlot
  subscribeDyn_ (void <<< updateList latestRef mainSlot handler) dynArray

dynamicList_ :: forall m a
   . MonadFRP m
  => MonadReplace m
  => Dynamic (Array a)
  -> (Dynamic a -> m Unit)
  -> m Unit
dynamicList_ dynArray handler = dynamicListWithIndex_ dynArray (\_ -> handler)
