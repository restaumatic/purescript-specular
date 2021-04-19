module Specular.FRPForm where
-- FRP Form POC, to run: npm run node-test && parcel build frpform/index.js

import Prelude

import Data.Char.Unicode (isLetter)
import Data.Maybe (Maybe (..), maybe)
import Data.String (length, null)
import Data.Traversable (for, intercalate)
import Effect (Effect)
import Specular.Dom.Element (dynText)
import Specular.Dom.Widget (runMainWidgetInBody)
import Specular.Lumiform.Form (assert, char, int, number, section, string, webform)
import Data.Semigroup
import Data.Monoid
import Control.Monad.Maybe.Trans
import Control.Monad.Writer.Trans
import Data.Tuple
import Data.Either
import Specular.Dom.Element (dynText, el, text)
import Data.String.Read as Read
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
import Specular.Dom.Builder.Class (domEventWithSample, elDynAttr')
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


-- Form as monad transformer stack on top of Dynamic

type Form a = MaybeT (WriterT Touch Dynamic) a

-- the history of user input of the form: whether it has been touched by a user input or remains intact
data Touch = Intact | Touched | Touching

instance touchSemigroup :: Semigroup Touch where
  append Touching _ = Touching
  append _ Touching = Touching
  append Touched  _ = Touched
  append _ Touched = Touched
  append _ _ = Intact

instance monoidTouch :: Monoid Touch where
  mempty = Intact

-- unwraping Dynamic of Form
formDynamic :: forall a . Form a -> Dynamic (Tuple (Maybe a) Touch)
formDynamic form = runWriterT (runMaybeT form)

whenFormTouched :: forall a m . MonadReplace m => MonadFRP m => Form a -> (Maybe a -> m Unit) -> m Unit
whenFormTouched form action = withDynamic_ (formDynamic form) case _ of
  Tuple ma Touched -> action ma
  _ -> pure unit

whenIntactNothing :: forall a m . MonadReplace m => MonadFRP m => Form a -> m Unit -> m Unit
whenIntactNothing form action = withDynamic_ (formDynamic form) case _ of
  Tuple Nothing Intact -> action
  _ -> pure unit

whenTouchedJust :: forall a m . MonadReplace m => MonadFRP m => Form a -> (a -> m Unit) -> m Unit
whenTouchedJust form action = withDynamic_ (formDynamic form) case _ of
  Tuple (Just a) Touched -> action a
  _ -> pure unit

-- these are the things we can do with From that we cannot do with plain Dynamic
justOf :: forall a . Form (Maybe a) -> Form a
justOf form = MaybeT $ WriterT $ do
  mmaw <-  runWriterT (runMaybeT form )
  pure $ case mmaw of
    Tuple (Just (Just a)) w -> Tuple (Just a) w
    Tuple _ w -> Tuple Nothing w

lastOf :: forall a . Form (Last a) -> Form a
lastOf form = MaybeT $ WriterT $ do
  mmaw <-  runWriterT (runMaybeT form )
  pure $ case mmaw of
    Tuple (Just (Last (Just a))) w -> Tuple (Just a) w
    Tuple _ w -> Tuple Nothing w

nothingOf :: forall a . Form (Maybe a) -> Form Unit
nothingOf form = MaybeT $ WriterT $ do
  mmaw <-  runWriterT (runMaybeT form)
  pure $ case mmaw of
    Tuple (Just (Just _)) w -> Tuple Nothing w
    Tuple _ w -> Tuple (Just unit) w

leftOf :: forall e a. Form (Either e a) -> Form e
leftOf f = justOf $ leftToMaybe <$> f
  where
    leftToMaybe = either Just (const Nothing)

rightOf :: forall e a . Form (Either e a) -> Form a
rightOf f = justOf $ rightToMaybe <$> f
  where
    rightToMaybe = either (const Nothing) Just

selection :: forall a . Eq a => Form a -> Form (Array a) -> Form (Maybe a)
selection selected options = selected >>= \a -> (find (_ `eq` a)) <$> options

-- Field - a primitive, atomic Form

type Field a =
  { inputValueRef :: Ref (Tuple a Touch)
  }

newField :: forall a . Monoid a => Effect (Field a)
newField = do
  ref <- new (Tuple mempty mempty)
  pure {inputValueRef: ref }

writeField :: forall a . Field a -> Callback (Tuple a Touch)
writeField input = (\a -> const a) >$< modify input.inputValueRef

readField :: forall a . Field a -> Form a
readField input = MaybeT $ WriterT $ do
  Tuple a w <- refValue input.inputValueRef
  pure (Tuple (Just a) w)

---

stringInputWidget :: Callback (Tuple String Touch) -> Widget Unit
stringInputWidget callback = do
  Tuple element _ <- elDynAttr' "input" (pure mempty) (pure unit)
  domChanged <- domEventWithSample (\_ -> getTextInputValue element) "input" element
  attachEvent domChanged ((\str -> Tuple str Touched) >$< callback)
  pure unit

selectInputWidget
  :: forall a .
  Eq a
  => Show a
  => BoundedEnum a
  => Form (Array a)
  -> Callback (Tuple (Last a) Touch)
  -> Widget Unit
selectInputWidget options callback = do
  withDynamic_ (formDynamic options) \dyn -> do
    let options = case dyn of
          (Tuple Nothing _) -> []
          (Tuple (Just options) _) -> options
    let toOption value = el "option" [attr "value" (show (fromEnum value))] $ text $ show value
    (Tuple selectEl _) <- el' "select" $ traverse_ toOption options
    domChanged <- domEventWithSample (\_ -> getTextInputValue selectEl) "change" selectEl
    attachEvent domChanged ((\str -> Tuple (Last (Just (unsafePartial (fromJust (toEnum (fromJust (fromString str))))))) Touched) >$< callback)
    pure unit

---

newInt :: String -> Either String Int
newInt s = case Int.fromString s of
  Nothing -> Left "Not an Int"
  (Just i) -> Right i

newtype Age = Age { ageToInt :: Int }

derive newtype instance showAge :: Show Age

newAge :: Int -> Either String Age
newAge i
  | i > 0 = Right $ Age { ageToInt: i }
  | otherwise = Left "must be positive"

newtype Name = Name { nameToString :: String }

derive newtype instance showName :: Show Name

newName :: String -> Either String Name
newName s
  | null s = Left "must be non empty"
  | otherwise = Right $ Name { nameToString: s }

data Title = Mr | Ms

derive instance ordTitle :: Ord Title

instance enumTitle :: Enum Title where
  succ Mr = Just Ms
  succ Ms = Nothing
  pred Mr = Nothing
  pred Ms = Just Mr

instance boundedTitle :: Bounded Title where
  bottom = Mr
  top = Ms

instance boundedEnumTitle :: BoundedEnum Title where
  cardinality = Cardinality 2
  toEnum 0 = Just Mr
  toEnum 1 = Just Ms
  toEnum _ = Nothing
  fromEnum Mr = 0
  fromEnum Ms = 1

instance showTitle :: Show Title where
  show Mr = "Mr"
  show Ms = "Ms"
derive instance eqTitle :: Eq Title

data ShirtSize = S | M | L | XL

derive instance ordShirtSize :: Ord ShirtSize

instance enumShirtSize :: Enum ShirtSize where
  succ S = Just M
  succ M = Just L
  succ L = Just XL
  succ XL = Nothing
  pred XL = Just L
  pred L = Just M
  pred M = Just S
  pred S = Nothing

instance bounded :: Bounded ShirtSize where
  bottom = S
  top = XL

instance boundedShirtSize :: BoundedEnum ShirtSize where
  cardinality = Cardinality 4
  toEnum 0 = Just S
  toEnum 1 = Just M
  toEnum 2 = Just L
  toEnum 3 = Just XL
  toEnum _ = Nothing
  fromEnum S = 0
  fromEnum M = 1
  fromEnum L = 2
  fromEnum XL = 3

instance showShirtSize :: Show ShirtSize where
  show S = "S"
  show M = "M"
  show L = "L"
  show XL = "XL"
derive instance eqShirtSize :: Eq ShirtSize

data Person = Person {
  personAge :: Age,
  personName :: Name,
  personTitle :: Title,
  personShirtSize :: ShirtSize
}

personForm :: Effect (Widget (Form Person))
personForm = do
  ageInput <- newField
  nameInput <- newField
  titleInput <- newField
  shirtSizeInput <- newField

  pure $ do
    let
      ageOrError = (newInt >=> newAge) <$> readField ageInput
      age = rightOf ageOrError
      ageError = leftOf ageOrError
      nameOrError = do
        v <- readField nameInput
        if null v
          then pure $ Left "must not be empty"
          else do
          (Age a) <- age
          if a.ageToInt  < 10 && length v > 10
            then pure $ Left "Too long name for such a young child"
            else pure $ Right $ Name { nameToString: v }
      name = rightOf nameOrError
      nameError = leftOf nameOrError
      allowedTitles = pure [Mr, Ms]
      maybeTitle = selection (lastOf (readField titleInput)) allowedTitles
      title = justOf maybeTitle
      titleError = "Title must be selected" <$ nothingOf maybeTitle
      allowedShirtSizes title = case title of
        Ms -> [S, M]
        Mr -> [M, L, XL]
      allowedSizes = allowedShirtSizes <$> title
      maybeShirtSize = selection (lastOf (readField shirtSizeInput)) allowedSizes
      shirtSizeError = "Shirt size must be selected" <$ nothingOf maybeShirtSize
      shirtSize = justOf maybeShirtSize
      person = (\a n t s -> Person { personAge: a, personName: n, personTitle: t, personShirtSize: s}) <$> age <*> name <*> title <*> shirtSize

    el "h1" [] $ text "Form Demo"
    whenIntactNothing person $ el "span" [attr "style" "color: green;"] $ text "Please fill in below"
    el "div" [] do
      el "span" [] $ text "Age"
      el "div" [] do
        stringInputWidget (writeField ageInput)
        whenIntactNothing age $ el "span" [attr "style" "color: green;"] $ text "obligatory"
        whenTouchedJust ageError $ el "span" [attr "style" "color: red;"] <<< text
    el "div" [] do
      el "span" [] $ text "Name"
      el "div" [] do
        stringInputWidget (writeField nameInput)
        whenIntactNothing name $ el "span" [attr "style" "color: green;"] $ text "obligatory"
        whenTouchedJust nameError $ el "span" [attr "style" "color: red;"] <<< text
    el "div" [] do
      el "span" [] $ text "Title"
      el "div" [] do
        selectInputWidget allowedTitles (writeField titleInput)
    el "div" [] do
      el "span" [] $ text "Shirt Size"
      el "div" [] do
        selectInputWidget allowedSizes (writeField shirtSizeInput)
    whenTouchedJust person $ const $ el "button" [] $ text "Submit"
    pure person

main :: Effect Unit
main = do
  f <- personForm
  _ <- runMainWidgetInBody f
  pure unit
