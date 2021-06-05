module Specular.FRPFormExample where
-- FRP Form POC example, to run: spago build && parcel build frpform/index.js

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
import Specular.Ref hiding (const)
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


mkInt :: String -> Either String Int
mkInt s = case Int.fromString s of
  Nothing -> Left "Not an Int"
  (Just i) -> Right i

newtype Age = Age { ageToInt :: Int }

showAge (Age age) = show age.ageToInt

mkAge :: Int -> Either String Age
mkAge i
  | i > 0 = Right $ Age { ageToInt: i }
  | otherwise = Left "must be positive"

newtype Name = Name { nameToString :: String }

mkName :: String -> Either String Name
mkName s
  | null s = Left "must be non empty"
  | otherwise = Right $ Name { nameToString: s }

data Person = Person {
  personAge :: Age,
  personName :: Name
}

showName (Name n) = n.nameToString


mkPersonForm :: Effect (Form Person (Tuple String String))
mkPersonForm = do
  ageField <- field
  nameField <- field
  repeatedNameField <- field

  let
    age = eitherOf $ (mkInt >=> mkAge) <$> fieldInput ageField
    name = eitherOf $ do
      v <- fieldInput nameField
      if null v
        then pure $ Left "must not be empty"
        else do
        (Age a) <- age
        if a.ageToInt  < 10 && length v > 10
          then pure $ Left "Too long name for such a young child"
          else pure $ Right $ Name { nameToString: v }
    repeatedName = eitherOf $ do
      (Name originalName) <- name
      repeatedName <- fieldInput repeatedNameField
      pure $ if originalName.nameToString == repeatedName
        then Right (Name originalName)
        else Left "Name mismatch"
    person = (\a n -> Person { personAge: a, personName: n}) <$> age <*> repeatedName

  pure do
    el "h1" [] $ text "Person Form"
    whenInputIntact person $ el "span" [attr "style" "color: green;"] $ text "Please fill in below"
    el "div" [] do
      text "Age"
      el "div" [] do
        stringFieldWidget ageField
        whenInputIntact age $ el "span" [attr "style" "color: green;"] $ text "mandatory"
        whenInputTouchedIncorrect age $ el "span" [attr "style" "color: red;"] <<< text
    el "div" [] do
      text "Name"
      el "div" [] do
        stringFieldWidget nameField
        whenInputIntact name $ el "span" [attr "style" "color: green;"] $ text "mandatory"
        whenInputTouchedIncorrect name $ el "span" [attr "style" "color: red;"] <<< text
    el "div" [] do
      text "Repeat Name"
      el "div" [] do
        stringFieldWidget repeatedNameField
        whenInputIntact repeatedName $ el "span" [attr "style" "color: green;"] $ text "mandatory"
        whenInputTouchedIncorrect repeatedName $ el "span" [attr "style" "color: red;"] <<< text
    whenInputCorrect person $ \(Person person) -> do
      el "div" [] do
        el "h2" [] $ text "Review"
        el "p" [] $ text $ "Age: " <> showAge person.personAge
        el "p" [] $ text $ "Name: " <> showName person.personName
    pure (Tuple person $ \(Tuple name age) -> do
      writeField ageField (Tuple age mempty)
      writeField nameField (Tuple name mempty))

main :: Effect Unit
main = do
  personForm <- mkPersonForm
  runMainWidgetInBody do
    (Tuple person populatePerson) <- el "div" [attr "style" "padding: 10px;"] personForm
    el "button" [onClick_ (populatePerson (Tuple "Eryk" "38"))] $ text "Populate"
    whenInputCorrect person $ \p -> el "button" [] $ text "Submit"
  pure unit
