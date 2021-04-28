module Specular.Ref
  ( Ref(..)
  , newRef
  , new
  , newRefWithEvent
  , value
  , modify
  , refUpdateConst
  , set
  , focusRef
  , pureFocusRef
  , previewRef
  , readRef

  , constRef

  , wrapViewWidget
  , Callback
  , Lens
  , Prism
  -- Deprecated, use `set` instead
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

-- | A handle that lets the owner trigger some action with payload of type `a`.
-- | Can be thought of as "inverse of `Event`".
-- |
type Callback a = (a -> Effect Unit)

data Ref a = Ref (Dynamic a) (Callback (a -> a))

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
refUpdate :: forall a. Ref a -> Callback (a -> a)
refUpdate = modify

-- | A Callback to modify value of this Ref using a function.
modify :: forall a. Ref a -> Callback (a -> a)
modify (Ref _ update) = update

-- | Old name for `set`.
refUpdateConst :: forall a. Ref a -> Callback a
refUpdateConst = set

-- | A Callback to overwrite value of this Ref.
set :: forall a. Ref a -> Callback a
set r = (\new _old -> new) >>> modify r

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


-- Old name for `set`
updateRef :: forall a. Ref a -> a -> Effect Unit
updateRef = set

readRef :: forall m a. MonadEffect m => Ref a -> m a
readRef (Ref value update) = pull $ readBehavior $ current value

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
