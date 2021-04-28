module Specular.FRPForm where
-- FRP Input POC, to run: pulp --psc-package build && parcel build frpform/index.js

import Prelude

import Data.Maybe (Maybe (..), maybe)
import Data.String (length, null)
import Data.Traversable (for, intercalate)
import Effect (Effect)
import Specular.Dom.Element (dynText)
import Specular.Dom.Widget (runMainWidgetInBody)
import Data.Semigroup
import Data.Monoid
import Control.Monad.Maybe.Trans
import Control.Monad.Writer.Trans
import Data.Tuple
import Data.Either
import Specular.Dom.Element (dynText, el, text)
import Data.Int as Int
import Data.Eq
import Data.Maybe
import Data.Maybe.Last
import Data.Newtype
import Data.Foldable
import Specular.FRP
import Specular.Ref
import Specular.Callback
import Data.Functor.Contravariant
import Specular.Dom.Builder.Class (domEventWithSample)
import Specular.Dom.Element (dynText, el, text)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Input (getTextInputValue, setTextInputValue)
import Control.Monad.Replace
import Specular.Dom.Element
import Data.Foldable (traverse_)
import Data.Enum
import Data.Int
import Specular.Dom.Builder.Class (el', elAttr')
import Partial.Unsafe (unsafePartial)
import Specular.Dom.Node.Class ((:=))


-- Input as monad transformer stack on top of Dynamic

type Input a = MaybeT (WriterT Touch Dynamic) a

-- MaybeT - introduces possible unavailability of form value due to validation failure
-- WriterT Touch - introduces out-of-band form data, form meta-data like intact/touched property
-- also played around with ReaderT but didn't find any useful application of it

-- the intact/touched propery of the form: whether it has been touched by a user input or remains intact
-- handled by WriterT tranformer, must be a Monoid
data Touch = Intact | Touched | Touching

instance touchSemigroup :: Semigroup Touch where
  append Touching _ = Touching
  append _ Touching = Touching
  append Touched _ = Touched
  append _ Touched = Touched
  append _ _ = Intact

instance monoidTouch :: Monoid Touch where
  mempty = Intact

-- unwraping Dynamic of a Input, then you can handle Dynamic as usual
inputDynamic :: forall a . Input a -> Dynamic (Tuple (Maybe a) Touch)
inputDynamic form = runWriterT (runMaybeT form)

-- or you can use shortcut functions like:
whenInputIntactNothing :: forall a m . MonadReplace m => MonadFRP m => Input a -> m Unit -> m Unit
whenInputIntactNothing form action = withDynamic_ (inputDynamic form) case _ of
  Tuple Nothing Intact -> action
  _ -> pure unit

whenInputTouchedJust :: forall a m . MonadReplace m => MonadFRP m => Input a -> (a -> m Unit) -> m Unit
whenInputTouchedJust form action = withDynamic_ (inputDynamic form) case _ of
  Tuple (Just a) Touched -> action a
  _ -> pure unit

whenInputJust :: forall a m . MonadReplace m => MonadFRP m => Input a -> (a -> m Unit) -> m Unit
whenInputJust form action = withDynamic_ (inputDynamic form) case _ of
  Tuple (Just a) _ -> action a
  _ -> pure unit

-- with functions in below one can manipulate Inputs
-- these are the things one cannot to with plain Dynamic
justOf :: forall a . Input (Maybe a) -> Input a
justOf form = MaybeT $ WriterT $ do
  mmaw <-  runWriterT (runMaybeT form )
  pure $ case mmaw of
    Tuple (Just (Just a)) w -> Tuple (Just a) w
    Tuple _ w -> Tuple Nothing w

lastOf :: forall a . Input (Last a) -> Input a
lastOf form = MaybeT $ WriterT $ do
  mmaw <-  runWriterT (runMaybeT form )
  pure $ case mmaw of
    Tuple (Just (Last (Just a))) w -> Tuple (Just a) w
    Tuple _ w -> Tuple Nothing w

nothingOf :: forall a . Input (Maybe a) -> Input Unit
nothingOf form = MaybeT $ WriterT $ do
  mmaw <-  runWriterT (runMaybeT form)
  pure $ case mmaw of
    Tuple (Just (Just _)) w -> Tuple Nothing w
    Tuple _ w -> Tuple (Just unit) w

leftOf :: forall e a. Input (Either e a) -> Input e
leftOf f = justOf $ leftToMaybe <$> f
  where
    leftToMaybe = either Just (const Nothing)

rightOf :: forall e a . Input (Either e a) -> Input a
rightOf f = justOf $ rightToMaybe <$> f
  where
    rightToMaybe = either (const Nothing) Just

selection :: forall a . Eq a => Input a -> Input (Array a) -> Input (Maybe a)
selection selected options = selected >>= \a -> (find (_ `eq` a)) <$> options

-- but how can we create an Input?

-- Field - a primitive, atomic Input

type Field a =
  { inputValueRef :: Ref (Tuple a Touch)
  }

newField :: forall a . Monoid a => Effect (Field a)
newField = do
  ref <- new (Tuple mempty mempty)
  pure {inputValueRef: ref }

writeField :: forall a . Field a -> Callback (Tuple a Touch)
writeField input = (\a -> const a) >$< modify input.inputValueRef

readField :: forall a . Field a -> Input a
readField input = MaybeT $ WriterT $ do
  Tuple a w <- value input.inputValueRef
  pure (Tuple (Just a) w)

-- then we can wrap Fields with Widgets

stringFieldWidget :: Field String -> Widget (Callback String)
stringFieldWidget field = do
  Tuple element _ <- elAttr' "input" mempty (pure unit)
  domChanged <- domEventWithSample (\_ -> getTextInputValue element) "input" element
  attachEvent domChanged ((\str -> Tuple str Touched) >$< writeField field)
  pure $ ((\str -> Tuple str Intact) >$< writeField field) <> mkCallback (setTextInputValue element)

-- this doens't work for now
selectFieldWidget ::
  forall a . Eq a => Show a => BoundedEnum a => Input (Array a)
  -> Field (Last a)
  -> Widget Unit
selectFieldWidget options field = do
  withDynamic_ (inputDynamic options) \dyn -> do
    let options = case dyn of
          (Tuple Nothing _) -> []
          (Tuple (Just options) _) -> options
    let toOption value = el "option" [attr "value" (show (fromEnum value))] $ text $ show value
    (Tuple selectEl _) <- el' "select" $ traverse_ toOption options
    domChanged <- domEventWithSample (\_ -> getTextInputValue selectEl) "change" selectEl
    attachEvent domChanged ((\str -> Tuple (Last (Just (unsafePartial (fromJust (toEnum (fromJust (fromString str))))))) Touched) >$< writeField field)
    pure unit

-- Form is a Widget that provides input and "callback" to modify input fields

type Form i o = Widget (Tuple (Input i) (Callback o))
