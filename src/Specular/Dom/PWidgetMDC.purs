module Specular.Dom.ComponentMDC
  ( button
  , filledText
  , checkbox
  ) where

import Prelude

import Data.Newtype (modify, wrap)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Specular.Dom.Browser (Node, (:=))
import Specular.Dom.Component (inside, onClick, static, text, textInput, withUniqDyn)
import Specular.Dom.Component as Specular
import Specular.FRP (never)

button :: forall f a. Applicative f => Specular.ComponentWrapper f a a -> Specular.ComponentWrapper f a a
button wrapped = 
  inside "button" (const $ "class" := "mdc-button mdc-button--raised foo-button") ((\dyn node -> (liftEffect $ mdcWith material.ripple."MDCRipple" node mempty) *> pure never) <> onClick) $
    (inside "div" (const $ "class" := "mdc-button__ripple") mempty mempty)
    <>
    (inside "span" (const $ "class" := "mdc-button__label") mempty wrapped)

filledText :: forall f. Applicative f => String -> Specular.ComponentWrapper f String String
filledText hintText = withUniqDyn $
  inside "label" (const $ "class" := "mdc-text-field mdc-text-field--filled") (\_ node -> (liftEffect $ mdcWith material.textField."MDCTextField" node mempty) *> pure never) $
    (inside "span" (const $ "class" := "mdc-text-field__ripple") mempty mempty)
    <>
    (inside "span" (const $ "class" := "mdc-floating-label" <> "id" := "my-label-id") mempty (text # static hintText))
    <>
    (textInput (const $ "class" := "mdc-text-field__input" <> "type" := "text" <> "aria-labelledby" := "my-label-id"))
    <>
    (inside "span" (const $ "class" := "mdc-line-ripple") mempty mempty)


checkbox :: forall f. Applicative f => Specular.ComponentWrapper f Boolean Boolean
checkbox = withUniqDyn $
  inside "div" (const $ "class" := "mdc-touch-target-wrapper") (\_ node -> (liftEffect $ mdcWith material.checkbox."MDCCheckbox" node mempty) *> pure never) $
    inside "div" (const $ "class" := "mdc-checkbox mdc-checkbox--touch") mempty $
      (Specular.checkbox (const $ "class" := "mdc-checkbox__native-control"))
      <>
      (inside "div" (const $ "class":= "mdc-checkbox__background") mempty $
        inside "svg" (const $ "class" := "mdc-checkbox__checkmark" <> "viewBox" := "0 0 24 24") mempty $
          (inside "path" (const $ "class" := "mdc-checkbox__checkmark-path" <> "fill" := "none" <> "d" := "M1.73,12.91 8.1,19.28 22.79,4.59") mempty mempty)
          <>
          (inside "div" (const $ "class" := "mdc-checkbox__mixedmark") mempty mempty)
      )
      <>
      (inside "div" (const $ "class" := "mdc-checkbox__ripple") mempty mempty)


---

foreign import data ComponentClass :: Type
foreign import data Component :: Type

foreign import material
  :: { textField :: { "MDCTextField" :: ComponentClass }
     , ripple :: { "MDCRipple" :: ComponentClass }
     , drawer :: { "MDCDrawer" :: ComponentClass }
     , tabBar :: { "MDCTabBar" :: ComponentClass }
     , dialog :: { "MDCDialog" :: ComponentClass }
     , snackbar :: { "MDCSnackbar" :: ComponentClass }
     , radio :: { "MDCRadio" :: ComponentClass }
     , chips :: { "MDCChip" :: ComponentClass }
     , select :: { "MDCSelect" :: ComponentClass }
     , list :: { "MDCList" :: ComponentClass }
     , checkbox :: { "MDCCheckbox" :: ComponentClass }
     }

foreign import _new :: EffectFn2 ComponentClass Node Component

mdcWith :: ComponentClass -> Node -> (Component -> Node -> Effect Unit) -> Effect Unit
mdcWith class_ node init = do
  component <- new class_ node
  pure unit
  -- Tuple _ cleanup <- (map fst <<< runCleanupT) $ init component node
  -- pushDelayed cleanups cleanup
  where
    new :: ComponentClass -> Node -> Effect Component
    new cls node = liftEffect $ runEffectFn2 _new cls node
