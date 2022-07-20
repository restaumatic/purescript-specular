module Specular.Dom.Element
  ( module X
  , el'
  , el
  , el_
  , Prop(..)

  , text
  , dynText

  , attr
  , attrD
  , attrWhen
  , attrUnless
  , attrWhenD
  , attrUnlessD

  , attrs
  , attrsD
  , attrsWhen
  , attrsUnless
  , attrsWhenD
  , attrsUnlessD

  , on
  , onClick
  , onClick_
  , preventDefault
  , stopPropagation

  , valueD
  , bindValueOnChange
  , bindValueOnInput

  , checkedD
  , bindChecked

  , ClassName
  , class_
  , classes
  , classesD
  , classWhenD
  , classUnlessD
  , classWhen
  , classUnless
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (foreachE, Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, mkEffectFn1, mkEffectFn2, mkEffectFn3, mkEffectFn4, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import Foreign.Object as Object
import Specular.Dom.Browser (EventType, Node, appendChild, createTextNode, setText, (:=))
import Specular.Dom.Browser as DOM
import Specular.Dom.Builder (mkBuilder', runBuilder')
import Specular.Dom.Builder.Class (BuilderEnv)
import Specular.Dom.Builder.Class (rawHtml) as X
import Specular.Dom.Node.Class (Attrs, TagName, createElement, removeAttributes, setAttributes)
import Specular.Dom.Widget (RWidget)
import Specular.Dom.Widgets.Input (getCheckboxChecked, getTextInputValue)
import Specular.FRP (Dynamic, _subscribeEvent, changed, readDynamic, subscribeDyn_)
import Effect.Ref (new, read, write)
import Specular.Internal.Effect (DelayedEffects, pushDelayed)
import Specular.Ref (Ref(..))
import Unsafe.Coerce (unsafeCoerce)
import Specular.Internal.Profiling as ProfilingInternal
import Specular.Profiling as Profiling

newtype Prop = Prop (EffectFn2 Node DelayedEffects Unit)

instance semigroupProp :: Semigroup Prop where
  append (Prop x) (Prop y) = Prop $ mkEffectFn2 \node cleanups -> do
    runEffectFn2 x node cleanups
    runEffectFn2 y node cleanups

instance monoidProp :: Monoid Prop where
  mempty = Prop $ mkEffectFn2 \_ _ -> pure unit

el' :: forall r a. TagName -> Array Prop -> RWidget r a -> RWidget r (Tuple Node a)
el' tagName props body = mkBuilder' $ mkEffectFn1 \env -> do
  node <- createElement tagName
  result <- runEffectFn4 initElement env node props body
  pure (Tuple node result)

el :: forall r a. TagName -> Array Prop -> RWidget r a -> RWidget r a
el tagName props body = mkBuilder' $ mkEffectFn1 \env -> do
  node <- createElement tagName
  runEffectFn4 initElement env node props body

initElement :: forall r a. EffectFn4 (BuilderEnv r) Node (Array Prop) (RWidget r a) a
initElement = mkEffectFn4 \env node props body -> do
  mark <- runEffectFn1 ProfilingInternal.begin "el"
  result <- runEffectFn2 runBuilder' (env { parent = node }) body
  foreachE props \(Prop prop) ->
    runEffectFn2 prop node env.cleanup
  appendChild node env.parent
  runEffectFn1 ProfilingInternal.end mark
  pure result

el_ :: forall r a. TagName -> RWidget r a -> RWidget r a
el_ tagName = el tagName mempty

-- Text node

text :: forall r. String -> RWidget r Unit
text str = mkBuilder' $ mkEffectFn1 \env -> do
  node <- createTextNode str
  appendChild node env.parent

dynText :: forall r. Dynamic String -> RWidget r Unit
dynText strD = do
  node <- mkBuilder' $ mkEffectFn1 \env -> do
    node <- createTextNode ""
    appendChild node env.parent
    pure node
  subscribeDyn_ (setText node) strD

-- * Attributes

-- | Attach static attributes to the node.
attrs :: Attrs -> Prop
attrs a = Prop $ mkEffectFn2 \node _ ->
  setAttributes node a

-- | Attach a dynamically-changing map of attributes to the node.
-- |
-- | The set of attributes must be distinct from the attributes set by other
-- | uses of `attrs` or `attrsD` on the same element.
attrsD :: Dynamic Attrs -> Prop
attrsD dynAttrs = Prop $ mkEffectFn2 \node cleanups -> do
  attrsRef <- new Object.empty
  let
    -- TODO: this could be implemented more efficiently with Brutal Mutability (TM)
    resetAttributes = mkEffectFn1 \newAttrs -> do
      oldAttrs <- read attrsRef
      write newAttrs attrsRef
      let
        changed = Object.filterWithKey (\k v -> Object.lookup k oldAttrs /= Just v) newAttrs
        removed = Array.filter (\k -> not (k `Object.member` newAttrs)) $ Object.keys oldAttrs

      removeAttributes node removed
      setAttributes node changed

  runEffectFn3 _subscribeDyn cleanups dynAttrs resetAttributes

_subscribeDyn :: forall a. EffectFn3 DelayedEffects (Dynamic a) (EffectFn1 a Unit) Unit
_subscribeDyn = mkEffectFn3 \cleanups dyn handler -> do
  unsub <- runEffectFn2 _subscribeEvent (runEffectFn1 handler) (changed dyn)
  pushDelayed cleanups unsub
  initialValue <- readDynamic dyn
  runEffectFn1 handler initialValue

type AttrName = String
type AttrValue = String

attr :: AttrName -> AttrValue -> Prop
attr name value = attrs (name := value)

attrD :: AttrName -> Dynamic AttrValue -> Prop
attrD name valD = attrsD $ valD <#> (name := _)

attrWhen :: Boolean -> AttrName -> AttrValue -> Prop
attrWhen true attrName attrValue = attr attrName attrValue
attrWhen false _ _ = mempty

attrUnless :: Boolean -> AttrName -> AttrValue -> Prop
attrUnless false attrName attrValue = attr attrName attrValue
attrUnless true _ _ = mempty

attrsWhen :: Boolean -> Attrs -> Prop
attrsWhen true = attrs
attrsWhen false = mempty

attrsUnless :: Boolean -> Attrs -> Prop
attrsUnless false = attrs
attrsUnless true = mempty

attrWhenD :: Dynamic Boolean -> AttrName -> AttrValue -> Prop
attrWhenD conditionD name value = attrsD $ conditionD <#> if _ then name := value else mempty

attrUnlessD :: Dynamic Boolean -> AttrName -> AttrValue -> Prop
attrUnlessD conditionD name value = attrsD $ conditionD <#> if _ then mempty else name := value

attrsWhenD :: Dynamic Boolean -> Attrs -> Prop
attrsWhenD conditionD attrs' = attrsD $ conditionD <#> if _ then attrs' else mempty

attrsUnlessD :: Dynamic Boolean -> Attrs -> Prop
attrsUnlessD conditionD attrs' = attrsD $ conditionD <#> if _ then mempty else attrs'

-- * Events

on :: EventType -> (DOM.Event -> Effect Unit) -> Prop
on eventType cb = Prop $ mkEffectFn2 \node _cleanups -> do
  _ <- DOM.addEventListener eventType (\e -> Profiling.measure ("event: " <> eventType) $ cb e) node
  -- Note: we don't actually need to detach the listener when cleaning up -
  -- the node will be removed anyway.
  pure unit

onClick :: (DOM.Event -> Effect Unit) -> Prop
onClick cb = on "click" cb

onClick_ :: Effect Unit -> Prop
onClick_ cb = onClick $ \_ -> cb

preventDefault :: DOM.Event -> Effect Unit
preventDefault = DOM.preventDefault

stopPropagation :: DOM.Event -> Effect Unit
stopPropagation = runEffectFn1 _stopPropagation

foreign import _stopPropagation :: EffectFn1 DOM.Event Unit

-- * Input value

-- | Attach dynamically-changing `value` property to an input element.
-- | The value can still be changed by user interaction.
-- |
-- | Only works on `<input>` and `<select>` elements.
valueD :: Dynamic String -> Prop
valueD = propertyD "value"

-- | Set up a two-way binding between the `value` of an `<input>` element,
-- | and the given `Ref`.
-- |
-- | The `Ref` will be updated on `change` event, i.e. at the end of user inteaction, not on every keystroke.
-- |
-- | Only works on input elements.
bindValueOnChange :: Ref String -> Prop
bindValueOnChange (Ref val update) =
  valueD val <> on "change" (withTargetValue (const >>> update))

-- | Set up a two-way binding between the `value` of an `<input>` element,
-- | and the given `Ref`.
-- |
-- | The `Ref` will be updated on `input` event, i.e. on every keystroke.
-- |
-- | Only works on input elements.
bindValueOnInput :: Ref String -> Prop
bindValueOnInput (Ref val update) =
  valueD val <> on "input" (withTargetValue (const >>> update))

withTargetValue :: (String -> Effect Unit) -> (DOM.Event -> Effect Unit)
withTargetValue cb = \event -> do
  value <- getTextInputValue (unsafeEventTarget event)
  cb value

unsafeEventTarget :: DOM.Event -> Node
unsafeEventTarget e = (unsafeCoerce e).target

-- | Attach dynamically-changing `checked` property to an input element.
-- | The value can still be changed by user interaction.
-- |
-- | Only works on input `type="checkbox"` and `type="radio"` elements.
checkedD :: Dynamic Boolean -> Prop
checkedD = propertyD "checked"

-- | Set up a two-way binding between the `checked` of an `<input>` element,
-- | and the given `Ref`.
-- |
-- | Only works on input `type="checkbox"` and `type="radio"` elements.
bindChecked :: Ref Boolean -> Prop
bindChecked (Ref val update) =
  checkedD val <> on "change" (withTargetChecked (const >>> update))

withTargetChecked :: (Boolean -> Effect Unit) -> (DOM.Event -> Effect Unit)
withTargetChecked cb = \event -> do
  value <- getCheckboxChecked (unsafeEventTarget event)
  cb value

-- | Attach a Dynamic to an arbitrary DOM property.
-- | The caller must ensure that the property exists and has the same type as the Dynamic.
propertyD :: forall a. String -> Dynamic a -> Prop
propertyD property dyn = Prop $ mkEffectFn2 \node cleanups ->
  runEffectFn3 _subscribeDyn cleanups dyn (mkEffectFn1 (\v -> runEffectFn3 setProperty node property v))

foreign import setProperty :: forall a. EffectFn3 Node String a Unit

-- * CSS classes

type ClassName = String

-- | Attach a single CSS class to the element.
-- |
-- | Note: if using this property:
-- | - the `class` attribute must not be used,
-- | - the provided class name must be distinct from class names used in other properties from this module.
class_ :: ClassName -> Prop
class_ cls = Prop $ mkEffectFn2 \node _ ->
  runEffectFn2 _addClass node cls

-- | Attach an array of CSS classes to the element.
-- |
-- | Note: if using this property:
-- | - the `class` attribute must not be used,
-- | - the provided class names must be distinct from class names used in other properties from this module.
classes :: Array ClassName -> Prop
classes clss = Prop $ mkEffectFn2 \node _ ->
  foreachE clss \cls ->
    runEffectFn2 _addClass node cls

foreign import _addClass :: EffectFn2 Node ClassName Unit

-- | Attach a dynamically-changing array of CSS classes to the element.
-- |
-- | Note: if using this property:
-- | - the `class` attribute must not be used,
-- | - the provided class names must be distinct from class names used in other properties from this module.
classesD :: Dynamic (Array ClassName) -> Prop
classesD clssD = Prop $ mkEffectFn2 \node cleanups -> do
  updateClasses <- runEffectFn1 _initClasses node
  runEffectFn3 _subscribeDyn cleanups clssD updateClasses

-- | Initialize an object for tracking changes to a class array. Returns function to update the class list.
foreign import _initClasses :: EffectFn1 Node (EffectFn1 (Array ClassName) Unit)

-- | Conditionally attach a CSS class to the element. The class will be present if the condition is true.
-- |
-- | Note: if using this property:
-- | - the `class` attribute must not be used,
-- | - the provided class name must be distinct from class names used in other properties from this module.
classWhenD :: Dynamic Boolean -> ClassName -> Prop
classWhenD enabled cls = classesD (enabled <#> if _ then [ cls ] else [])

-- | `classUnlessD cond` = `classWhenD (not cond)`
classUnlessD :: Dynamic Boolean -> ClassName -> Prop
classUnlessD enabled cls = classesD (enabled <#> if _ then [] else [ cls ])

classWhen :: Boolean -> ClassName -> Prop
classWhen enabled cls = if enabled then class_ cls else mempty

classUnless :: Boolean -> ClassName -> Prop
classUnless enabled cls = if enabled then mempty else class_ cls
