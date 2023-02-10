module Specular.Dom.ComponentMDC
  ( button
  , filledText
  , checkbox
  , radioButton
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Specular.Dom.Browser (Node, (:=))
import Specular.Dom.Component (inside, onClick, static, text, textInput, withUniqDyn)
import Specular.Dom.Component as Specular
import Specular.FRP (never)

button :: forall f a. Applicative f => Specular.ComponentWrapper f a a -> Specular.ComponentWrapper f a a
button wrapped = 
  inside "button" (const $ "class" := "mdc-button mdc-button--raised foo-button") ((\_ node -> (liftEffect $ mdcWith material.ripple."MDCRipple" node mempty) *> pure never) <> onClick) $
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
  inside "div" (const $ "class" := "mdc-touch-target-wrapper") mempty $
    inside "div" (const $ "class" := "mdc-checkbox mdc-checkbox--touch") (\_ node -> (liftEffect $ mdcWith material.checkbox."MDCCheckbox" node mempty) *> pure never) $
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

radioButton :: forall f. Applicative f => Specular.ComponentWrapper f Boolean Boolean
radioButton = withUniqDyn $
  inside "div" (const $ "class" := "mdc-form-field") mempty
  (
    (inside "div" (const $ "class" := "mdc-radio") (\_ node -> (liftEffect $ mdcWith material.radio."MDCRadio" node mempty) *> pure never) $
      (Specular.radio (const $ "class" := "mdc-radio__native-control" <> "id" := "radio-1"))
      <>
      (inside "div" (const $ "class" := "mdc-radio__background") mempty $
        inside "div" (const $ "class" := "mdc-radio__outer-circle") mempty mempty
        <>
        inside "div" (const $ "class" := "mdc-radio__inner-circle") mempty mempty
      )
      <>
      (inside "div" (const $ "class" := "mdc-radio__ripple") mempty mempty)
    )
    -- <>
    -- (inside "label" (const $ "for" := "radio-1") (\_ node -> (liftEffect $ mdcWith material.formField."MDCFormField" node mempty) *> pure never) $ text # static "Radio 1")
  )
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
     , formField :: { "MDCFormField" :: ComponentClass }
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
