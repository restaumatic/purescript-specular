module Specular.Ref
  ( Ref(..)
  , new
  , newWithEvent
  , const
  , value
  , modify
  , read
  , write
  , focusRef
  , pureFocusRef
  , previewRef
  , wrapViewWidget
  , Lens
  , Prism
  --
  -- Deprecated, use `new` instead
  , newRef
  -- Deprecated, use `newWithEvent` instead
  , newRefWithEvent
  -- Deprecated, use `const` instead
  , constRef
  -- Deprecated, use `read` instead
  , readRef
  -- Deprecated, use `write` instead
  , updateRef
  -- Deprecated, use `write` instead
  , set
  -- Deprecated, use `write` instead
  , refUpdateConst
  -- Deprecated, use `value` instead
  , refValue
  -- Deprecated, use `modify` instead
  , refUpdate
  ) where

import Prelude hiding (const)

import Control.Apply (lift2)
import Data.Functor.Invariant (class Invariant)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Prelude as Prelude
import Specular.Dom.Widget (class MonadWidget)
import Specular.FRP (class MonadFRP, Dynamic, Event, WeakDynamic, newDynamic, readDynamic, subscribeEvent_, weaken)

data Ref a = Ref (Dynamic a) ((a -> a) -> Effect Unit)

instance invariantRef :: Invariant Ref where
  imap f g (Ref v update) = Ref (f <$> v) ((\h -> g <<< h <<< f) >>> update)


-- | Create a new Ref with an initial value.
new :: forall m a. MonadEffect m => a -> m (Ref a)
new initial = do
  {dynamic, modify: modify_} <- newDynamic initial
  pure $ Ref dynamic modify_

newWithEvent :: forall m a. MonadFRP m => a -> Event (a -> a) -> m (Ref a)
newWithEvent initial extraUpdate = do
  {dynamic, modify: modify_} <- newDynamic initial
  subscribeEvent_ modify_ extraUpdate
  pure $ Ref dynamic modify_


-- | The current value of the Ref, as a Dynamic.
value :: forall a. Ref a -> Dynamic a
value (Ref v _) = v


-- | Modify value of this Ref using a function.
modify :: forall a m. MonadEffect m => Ref a -> (a -> a) -> m Unit
modify (Ref _ update) = liftEffect <<< update


-- | Overwrite value of this Ref.
write :: forall a m. MonadEffect m => Ref a -> a -> m Unit
write r = (\new_ _old -> new_) >>> modify r


-- | Read the current value of a Ref
read :: forall m a. MonadEffect m => Ref a -> m a
read (Ref value_ _update) = readDynamic value_

-- | Create a Ref with a value
const  :: forall a. a -> Ref a
const x = Ref (pure x) (Prelude.const (pure unit))




type Lens s a = { get :: s -> a, set :: s -> a -> s }
type Prism s a = { preview :: s -> Maybe a, review :: a -> s }

focusRef :: forall s a. Dynamic (Lens s a) -> Ref s -> Ref a
focusRef lensD (Ref value_ update) =
  Ref
    (lift2 _.get lensD value_)
    (\x -> do
      f <- readDynamic (lensD <#> \lens modify_a s -> lens.set s (modify_a (lens.get s)))
      update (f x)
    )

pureFocusRef :: forall s a. Lens s a -> Ref s -> Ref a
pureFocusRef lens (Ref value_ update) =
  Ref
    (map lens.get value_)
    (
      (\modify_a s ->
        lens.set s (modify_a (lens.get s)))
      >>> update)

previewRef :: forall s a. Prism s a -> Ref s -> Ref (Maybe a)
previewRef prism (Ref value_ update) =
  Ref
    (map prism.preview value_)
    (
      (\modify_a s ->
          case (modify_a <<< prism.preview) s of
            (Just a) -> prism.review a
            _ -> s
      ) >>> update)




wrapViewWidget
  :: forall m a
   . MonadWidget m
  => (WeakDynamic a -> m (Event a))
  -> Ref a -> m Unit
wrapViewWidget widget r@(Ref value_ _update) = do
  updateE <- widget (weaken value_)
  subscribeEvent_ (set r) updateE



-- | Old name for `new`.
newRef :: forall m a. MonadEffect m => a -> m (Ref a)
newRef = new

-- | Old name for `newWithEvent`
newRefWithEvent :: forall m a. MonadFRP m => a -> Event (a -> a) -> m (Ref a)
newRefWithEvent = newWithEvent

-- | Old name for `value`
refValue :: forall a. Ref a -> Dynamic a
refValue = value

-- | Old name for `modify`.
refUpdate :: forall a m. MonadEffect m => Ref a -> (a -> a) -> m Unit
refUpdate = modify

-- | Old name for `write`
set :: forall a m. MonadEffect m => Ref a -> a -> m Unit
set = write

-- Old name for `write`
updateRef :: forall a m. MonadEffect m => Ref a -> a -> m Unit
updateRef = write

-- | Old name for `write`.
refUpdateConst :: forall a m. MonadEffect m => Ref a -> a -> m Unit
refUpdateConst = write


-- | Old name for `read`
readRef :: forall m a. MonadEffect m => Ref a -> m a
readRef = read


-- | Old name for `const`
constRef :: forall a. a -> Ref a
constRef = const
