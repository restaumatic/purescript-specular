module Specular.FRPForm where
-- FRP Form POC, to run: npm run node-test && parcel build frpform/index.js

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


-- Form as monad transformer stack on top of Dynamic

type Form a = MaybeT (WriterT Touch Dynamic) a

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

-- unwraping Dynamic of a Form, then you can handle Dynamic as usual
formDynamic :: forall a . Form a -> Dynamic (Tuple (Maybe a) Touch)
formDynamic form = runWriterT (runMaybeT form)

-- or you can use shortcut functions like:
whenFormIntactNothing :: forall a m . MonadReplace m => MonadFRP m => Form a -> m Unit -> m Unit
whenFormIntactNothing form action = withDynamic_ (formDynamic form) case _ of
  Tuple Nothing Intact -> action
  _ -> pure unit

whenFormTouchedJust :: forall a m . MonadReplace m => MonadFRP m => Form a -> (a -> m Unit) -> m Unit
whenFormTouchedJust form action = withDynamic_ (formDynamic form) case _ of
  Tuple (Just a) Touched -> action a
  _ -> pure unit

-- with functions in below one can manipulate Forms
-- these are the things one cannot to with plain Dynamic
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

-- but how can we create a Form?

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
  Tuple a w <- value input.inputValueRef
  pure (Tuple (Just a) w)

---

stringInputWidget :: Field String -> Widget (Callback String)
stringInputWidget field = do
  Tuple element _ <- elAttr' "input" mempty (pure unit)
  domChanged <- domEventWithSample (\_ -> getTextInputValue element) "input" element
  attachEvent domChanged ((\str -> Tuple str Touched) >$< writeField field)
  pure $ ((\str -> Tuple str Intact) >$< writeField field) <> mkCallback (setTextInputValue element)

selectInputWidget ::
  forall a . Eq a => Show a => BoundedEnum a => Form (Array a)
  -> Field (Last a)
  -> Widget Unit
selectInputWidget options field = do
  withDynamic_ (formDynamic options) \dyn -> do
    let options = case dyn of
          (Tuple Nothing _) -> []
          (Tuple (Just options) _) -> options
    let toOption value = el "option" [attr "value" (show (fromEnum value))] $ text $ show value
    (Tuple selectEl _) <- el' "select" $ traverse_ toOption options
    domChanged <- domEventWithSample (\_ -> getTextInputValue selectEl) "change" selectEl
    attachEvent domChanged ((\str -> Tuple (Last (Just (unsafePartial (fromJust (toEnum (fromJust (fromString str))))))) Touched) >$< writeField field)
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

personForm :: Effect (Widget (Tuple (Form Person) (Callback (Tuple String String))))
personForm = do
  ageInput <- newField
  nameInput <- newField
  titleInput <- newField
  shirtSizeInput <- newField

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

  pure do
    el "h1" [] $ text "Form Demo"
    whenFormIntactNothing person $ el "span" [attr "style" "color: green;"] $ text "Please fill in below"
    setAge <- el "div" [] do
      text "Age"
      el "div" [] do
        set <- stringInputWidget ageInput
        whenFormIntactNothing age $ el "span" [attr "style" "color: green;"] $ text "obligatory"
        whenFormTouchedJust ageError $ el "span" [attr "style" "color: red;"] <<< text
        pure set
        -- el "button" [onClick_ ((const "" ) >$< foo)] $ text "Clear"
    setName <- el "div" [] do
      text "Name"
      el "div" [] do
        set <- stringInputWidget nameInput
        whenFormIntactNothing name $ el "span" [attr "style" "color: green;"] $ text "obligatory"
        whenFormTouchedJust nameError $ el "span" [attr "style" "color: red;"] <<< text
        pure set
    el "div" [] do
      text "Title"
      el "div" [] do
        selectInputWidget allowedTitles titleInput
    el "div" [] do
      text "Shirt Size"
      el "div" [] do
        selectInputWidget allowedSizes shirtSizeInput
    whenFormTouchedJust person $ const $ el "button" [] $ text "Submit"
    pure (Tuple person $ mkCallback $ \(Tuple name age) -> do
      triggerCallback setAge age
      triggerCallback setName name)

main :: Effect Unit
main = do
  f <- personForm
  (Tuple person set) <- runMainWidgetInBody $ f
  triggerCallback set (Tuple "Eryk" "37")
  pure unit
