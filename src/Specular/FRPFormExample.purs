module Specular.FRPFormExample where

import Prelude

import Specular.FRPForm

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

-- Form is a Widget that provides input and "callback" to modify input fields

type Form i o = Widget (Tuple (Input i) (Callback o))

personForm :: Effect (Form Person (Tuple String String))
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
    el "h1" [] $ text "Input Demo"
    whenInputIntactNothing person $ el "span" [attr "style" "color: green;"] $ text "Please fill in below"
    setAge <- el "div" [] do
      text "Age"
      el "div" [] do
        set <- stringFieldWidget ageInput
        whenInputTouchedJust ageError $ el "span" [attr "style" "color: red;"] <<< text
        el "button" [onClick_ ((const "" ) >$< set)] $ text "Clear"
        pure set
    setName <- el "div" [] do
      text "Name"
      el "div" [] do
        set <- stringFieldWidget nameInput
        whenInputTouchedJust nameError $ el "span" [attr "style" "color: red;"] <<< text
        el "button" [onClick_ ((const "" ) >$< set)] $ text "Clear"
        pure set
    el "div" [] do
      text "Title"
      el "div" [] do
        selectFieldWidget allowedTitles titleInput
    el "div" [] do
      text "Shirt Size"
      el "div" [] do
        selectFieldWidget allowedSizes shirtSizeInput
    whenInputTouchedJust person $ const $ el "button" [] $ text "Submit"
    pure (Tuple person $ mkCallback $ \(Tuple name age) -> do
      triggerCallback setAge age
      triggerCallback setName name)

main :: Effect Unit
main = do
  f <- personForm
  (Tuple person setNameAndAge) <- runMainWidgetInBody $ f
  triggerCallback setNameAndAge (Tuple "Eryk" "37")
  pure unit
