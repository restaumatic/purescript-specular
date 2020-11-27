module Specular.Lumiform.Lumiform where

import Prelude

import Control.Applicative.Free (FreeAp, foldFreeAp, liftFreeAp)
import Data.Either (Either, either)
import Data.Functor.Contravariant ((>$<))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Specular.Callback (attachEvent)
import Specular.Dom.Builder.Class (domEventWithSample, elDynAttr')
import Specular.Dom.Element (dynText, el, text)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Input (getTextInputValue, setTextInputValue)
import Specular.FRP (Dynamic, subscribeDyn_)
import Specular.Ref (Ref(..), new)

textInputWidget :: (String -> Either String String) -> Widget (Dynamic (Maybe String))
textInputWidget validation = do
  ref@(Ref dyn updateRef) <- new ""
  Tuple element _ <- elDynAttr' "input" (pure mempty) (pure unit)
  Tuple velement _ <- elDynAttr' "span" (pure mempty) (pure unit)
  subscribeDyn_ (setTextInputValue element) dyn
  domChanged <- domEventWithSample (\_ -> getTextInputValue element) "input" element
  attachEvent domChanged (const >$< updateRef)
  dynText $ either identity (const "") <<< validation <$> dyn
  pure $ (\s -> either (const Nothing) (const (Just s)) (validation s)) <$> dyn

-- DSL
data LumiformF a =
    Output String a
  | Section String a
  | TextInput String (String -> Either String String) (String -> a)

instance functorLumiformF :: Functor LumiformF where
  map f (Output s a) = Output s (f a)
  map f (Section s a) = Section s (f a)
  map f (TextInput l v g) = TextInput l v (f <<< g)

type Lumiform = FreeAp LumiformF

output :: String -> Lumiform Unit
output str = liftFreeAp $ Output str unit

section :: String -> Lumiform Unit
section s = liftFreeAp $ Section s unit

textInput :: String -> (String -> Either String String) -> Lumiform String
textInput label validation = liftFreeAp $ TextInput label validation identity

-- interpreter
data Form a = Form (Widget (Dynamic (Maybe a)))

unform :: forall a . Form a -> Widget (Dynamic (Maybe a))
unform (Form w) = w

instance formFunctor :: Functor Form where
  map f (Form w)= Form (map (map (map f)) w)

instance formApplicative :: Applicative Form where
  pure = Form <<< pure <<< pure <<< pure

instance formApply :: Apply Form where
  apply (Form wdf) (Form wda) = Form $ (\dmf dma -> (<*>) <$> dmf <*> dma) <$> wdf <*> wda

lumiform' :: forall a . Lumiform a -> Form a
lumiform' = foldFreeAp go
  where
    go :: forall a . LumiformF a -> Form a
    go (Output str a) = Form $ do
      el "div" [] $ text str
      pure (pure (pure a))
    go (TextInput label (validate :: String -> Either String String) (f :: String -> a)) = Form $ do
      el "div" [] do
        text label
        el "div" [] do
          dyn <- textInputWidget validate
          pure (map f <$> dyn)
    go (Section s a) = Form $ do
      el "h1" [] $ text s
      pure $ pure $ pure a

form :: forall a . Lumiform a -> Widget (Dynamic (Maybe a))
form = unform <<< lumiform'