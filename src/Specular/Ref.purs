module Specular.Ref
  ( Ref(..)
  , new
  , newRefWithEvent
  , value
  , modify
  , read
  , write
  , refUpdateConst
  , focusRef
  , pureFocusRef
  , previewRef
  , constRef
  , wrapViewWidget
  , Lens
  , Prism
  -- Deprecated, use `write` instead
  , set
  -- Deprecated, use `new` instead
  , newRef
  -- Deprecated, use `read` instead
  , readRef
  -- Deprecated, use `write` instead
  , updateRef
  -- Deprecated, use `value` instead
  , refValue
  -- Deprecated, use `modify` instead
  , refUpdate
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Functor.Invariant (class Invariant)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Specular.Dom.Widget (class MonadWidget)
import Specular.FRP (class MonadFRP, Dynamic, Event, WeakDynamic, current, newDynamic, pull, readBehavior, readDynamic, subscribeEvent_, weaken)

data Ref a = Ref (Dynamic a) ((a -> a) -> Effect Unit)

instance invariantRef :: Invariant Ref where
  imap f g (Ref v update) = Ref (f <$> v) ((\h -> g <<< h <<< f) >>> update)

-- | Old name for `new`.
newRef :: forall m a. MonadEffect m => a -> m (Ref a)
newRef = new

-- | Create a new Ref with an initial value.
new :: forall m a. MonadEffect m => a -> m (Ref a)
new initial = do
  {dynamic, modify} <- newDynamic initial
  pure $ Ref dynamic modify

newRefWithEvent :: forall m a. MonadFRP m => a -> Event (a -> a) -> m (Ref a)
newRefWithEvent initial extraUpdate = do
  {dynamic, modify} <- newDynamic initial
  subscribeEvent_ modify extraUpdate
  pure $ Ref dynamic modify

-- | Old name for `value`
refValue :: forall a. Ref a -> Dynamic a
refValue = value

-- | The current value of the Ref, as a Dynamic.
value :: forall a. Ref a -> Dynamic a
value (Ref v _) = v

-- | Old name for `modify`.
refUpdate :: forall a. Ref a -> (a -> a) -> Effect Unit
refUpdate = modify

-- | Modify value of this Ref using a function.
modify :: forall a. Ref a -> (a -> a) -> Effect Unit
modify (Ref _ update) = update

-- | Old name for `set`.
refUpdateConst :: forall a. Ref a -> a -> Effect Unit
refUpdateConst = set

-- | Overwrite value of this Ref.
write :: forall a. Ref a -> a -> Effect Unit
write r = (\new _old -> new) >>> modify r

-- | Overwrite value of this Ref. Old name for `write`
set :: forall a. Ref a -> a -> Effect Unit
set = write

-- Old name for `write`
updateRef :: forall a. Ref a -> a -> Effect Unit
updateRef = write

type Lens s a = { get :: s -> a, set :: s -> a -> s }
type Prism s a = { preview :: s -> Maybe a, review :: a -> s }

focusRef :: forall s a. Dynamic (Lens s a) -> Ref s -> Ref a
focusRef lensD (Ref value update) =
  Ref
    (lift2 _.get lensD value)
    (\x -> do
      f <- readDynamic (lensD <#> \lens modify_a s -> lens.set s (modify_a (lens.get s)))
      update (f x)
    )

pureFocusRef :: forall s a. Lens s a -> Ref s -> Ref a
pureFocusRef lens (Ref value update) =
  Ref
    (map lens.get value)
    (
      (\modify_a s ->
        lens.set s (modify_a (lens.get s)))
      >>> update)

previewRef :: forall s a. Prism s a -> Ref s -> Ref (Maybe a)
previewRef prism (Ref value update) =
  Ref
    (map prism.preview value)
    (
      (\modify_a s ->
          case (modify_a <<< prism.preview) s of
            (Just a) -> prism.review a
            _ -> s
      ) >>> update)


-- | Read the current value of a Ref
read :: forall m a. MonadEffect m => Ref a -> m a
read (Ref value update) = pull $ readBehavior $ current value


-- | Old name for `read`
readRef :: forall m a. MonadEffect m => Ref a -> m a
readRef = read

wrapViewWidget
  :: forall m a
   . MonadWidget m
  => (WeakDynamic a -> m (Event a))
  -> Ref a -> m Unit
wrapViewWidget widget r@(Ref value update) = do
  updateE <- widget (weaken value)
  subscribeEvent_ (set r) updateE

constRef :: forall a. a -> Ref a
constRef x = Ref (pure x) (const (pure unit))
