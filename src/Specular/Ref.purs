module Specular.Ref
  ( Ref(..)
  , newRef
  , newRefWithEvent
  , refValue
  , refUpdate
  , refUpdateConst
  , focusRef
  , pureFocusRef
  , previewRef
  , updateRef
  , readRef

  , constRef

  , wrapViewWidget

  , Lens
  , Prism
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Functor.Contravariant (cmap, (>$<))
import Data.Functor.Invariant (class Invariant)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Specular.Callback (Callback, attachEvent, contramapCallbackDyn, mkCallback, nullCallback, triggerCallback)
import Specular.Dom.Widget (class MonadWidget)
import Specular.FRP (class MonadFRP, Dynamic, Event, WeakDynamic, current, newDynamic, pull, readBehavior, subscribeEvent_, weaken)


data Ref a = Ref (Dynamic a) (Callback (a -> a))

instance invariantRef :: Invariant Ref where
  imap f g (Ref value update) = Ref (f <$> value) (cmap (\h -> g <<< h <<< f) update)

newRef :: forall m a. MonadEffect m => a -> m (Ref a)
newRef initial = do
  {dynamic, modify} <- newDynamic initial
  pure $ Ref dynamic (mkCallback modify)

newRefWithEvent :: forall m a. MonadFRP m => a -> Event (a -> a) -> m (Ref a)
newRefWithEvent initial extraUpdate = do
  {dynamic, modify} <- newDynamic initial
  subscribeEvent_ modify extraUpdate
  pure $ Ref dynamic (mkCallback modify)

refValue :: forall a. Ref a -> Dynamic a
refValue (Ref value _) = value

refUpdate :: forall a. Ref a -> Callback (a -> a)
refUpdate (Ref _ update) = update

refUpdateConst :: forall a. Ref a -> Callback a
refUpdateConst = cmap (\new _old -> new) <<< refUpdate

type Lens s a = { get :: s -> a, set :: s -> a -> s }
type Prism s a = { preview :: s -> Maybe a, review :: a -> s }

focusRef :: forall s a. Dynamic (Lens s a) -> Ref s -> Ref a
focusRef lensD (Ref value update) =
  Ref
    (lift2 _.get lensD value)
    (contramapCallbackDyn
      (lensD <#> \lens modify_a s ->
        lens.set s (modify_a (lens.get s)))
      update)

pureFocusRef :: forall s a. Lens s a -> Ref s -> Ref a
pureFocusRef lens (Ref value update) =
  Ref
    (map lens.get value)
    (cmap
      (\modify_a s ->
        lens.set s (modify_a (lens.get s)))
      update)

previewRef :: forall s a. Prism s a -> Ref s -> Ref (Maybe a)
previewRef prism (Ref value update) =
  Ref
    (map prism.preview value)
    (cmap
      (\modify_a s ->
          case (modify_a <<< prism.preview) s of
            (Just a) -> prism.review a
            _ -> s
      ) update)


updateRef :: forall a. Ref a -> a -> Effect Unit
updateRef (Ref _ update) = triggerCallback update <<< const

readRef :: forall m a. MonadEffect m => Ref a -> m a
readRef (Ref value update) = pull $ readBehavior $ current value

wrapViewWidget
  :: forall m a
   . MonadWidget m
  => (WeakDynamic a -> m (Event a))
  -> Ref a -> m Unit
wrapViewWidget widget (Ref value update) = do
  updateE <- widget (weaken value)
  attachEvent updateE $ const >$< update

constRef :: forall a. a -> Ref a
constRef x = Ref (pure x) nullCallback
