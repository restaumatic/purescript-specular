module Specular.Lumiform.Lumiform where

import Prelude

import Control.Applicative.Free (FreeAp, foldFreeAp, liftFreeAp)
import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Replace (class MonadReplace, newSlot, replaceSlot)
import Control.Monad.Writer (WriterT(..), lift)
import Data.Date (weekday)
import Data.Either (Either)
import Data.Functor.Contravariant ((>$<))
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Prelude (class Functor, Unit, discard, identity, pure, unit, ($), (<<<))
import Specular.Callback (attachEvent)
import Specular.Dom.Browser (Attrs)
import Specular.Dom.Builder.Class (class MonadDomBuilder, domEventWithSample, elDynAttr')
import Specular.Dom.Element (el, el_, text)
import Specular.Dom.Widget (class MonadWidget, Widget, runMainWidgetInBody)
import Specular.Dom.Widgets.Input (getTextInputValue, setTextInputValue)
import Specular.FRP (class MonadFRP, Dynamic, WeakDynamic, newDynamic, subscribeDyn, subscribeDyn_, withDynamic_)
import Specular.Ref (Ref(..), new)

-- utils
-- hole :: forall a . a
-- hole = unsafeThrow "hole"

textInput :: Widget (Dynamic String)
textInput = do
  ref@(Ref dyn updateRef) <- new ""
  Tuple element _ <- elDynAttr' "input" (pure mempty) (pure unit)
  subscribeDyn_ (setTextInputValue element) dyn
  domChanged <- domEventWithSample (\_ -> getTextInputValue element) "input" element
  attachEvent domChanged (const >$< updateRef)
  pure dyn

-- DSL
data RequiredOrOptional = Required | Optional

data LumiformF a =
    Output String a
  | Section String a
  | Input (String -> a)
  -- | Text (Dynamic String -> a)

type Lumiform = FreeAp LumiformF

instance functorLumiformF :: Functor LumiformF where
  map f (Output s a) = Output s (f a)
  map f (Section s a) = Section s (f a)
  map f (Input g) = Input (f <<< g)
  -- map f (Field label ro validation input a) = Field label ro validation input (f a)

output :: String -> Lumiform Unit
output str = liftFreeAp $ Output str unit

section :: String -> Lumiform Unit
section s = liftFreeAp $ Section s unit

input :: Lumiform String
input = liftFreeAp $ Input identity

data Form a = Form (Widget (Dynamic a))

unform :: forall a . Form a -> Widget (Dynamic a)
unform (Form w) = w

instance formFunctor :: Functor Form where
  map f (Form w)= Form (map (map f) w)

instance formApplicative :: Applicative Form where
  pure = Form <<< pure <<< pure

instance formApply :: Apply Form where
  apply (Form wdf) (Form wda) = Form $ (<*>) <$> wdf <*> wda

lumiform' :: forall a . Lumiform a -> Form a
lumiform' = foldFreeAp go
  where
    go :: forall a . LumiformF a -> Form a
    go (Output str a) = Form $ do
      el "div" [] $ text str
      pure (pure a)
    go (Input (f :: String -> a)) = Form $ do
      dyn <- textInput
      pure (f <$> dyn)
    go (Section s a) = Form $ do
      el "div" [] $ text s
      pure (pure a)

lumiform :: forall a . Lumiform a -> Widget (Dynamic a)
lumiform = unform <<< lumiform'