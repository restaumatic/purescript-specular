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
import Control.Monad.Error.Class
import Control.Monad.Except.Trans
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
import Specular.Dom.Builder.Class (el', elAttr', elDynAttr')
import Specular.Dom.Node.Class ((:=))
import Specular.Dom.Browser


mkInt :: String -> Either String Int
mkInt s = case Int.fromString s of
  Nothing -> Left "Not an Int"
  (Just i) -> Right i

mkAge :: Int -> Either String Int
mkAge i
  | i > 0 = Right i
  | otherwise = Left "must be positive"

mkName :: String -> Either String String
mkName s
  | null s = Left "must be non empty"
  | otherwise = Right s

data Person = Person {
  personAge :: Int,
  personName :: String
}

mkPersonForm :: Effect (Form Person (Tuple String String))
mkPersonForm = do
  ageField <- field
  nameField <- field
  repeatedNameField <- field

  let
    ageInput = eitherOf' $ do
      age <- lift $ fieldInput ageField
      when (null age) $ throwError "must not be empty"
      except $ (mkInt >=> mkAge) age
    color :: forall a . Input a -> Dynamic Attrs
    color input = inputDynamic input <#> (\x -> case x of
      Tuple (Just (Right _)) _ -> "style" := "border: 1px solid green;"
      Tuple (Just (Left _)) Touched -> "style" := "border: 1px solid red;"
      _ -> "style" := "border: 1px solid black;")
    nameInput = eitherOf' $ do
      name <- lift $ fieldInput nameField
      when (null name) $ throwError "must not be empty"
      age <- lift $ ageInput
      when (age < 10 && length name > 10) $ throwError "Too long nameInput for such a young child"
      pure name
    repeatedName = eitherOf' $ do
      name <- lift $ nameInput
      repeatedName <- lift $ fieldInput repeatedNameField
      when (name /= repeatedName) $ throwError "Name mismatch"
      pure repeatedName
    personInput = (\a n -> Person { personAge: a, personName: n}) <$> ageInput <*> repeatedName

  pure do
    el "h1" [] $ text "Person Form"
    whenInputIntact personInput $ el "span" [attr "style" "color: green;"] $ text "Please fill in below"
    el "div" [] do
      text "Age"
      el "div" [] do
        stringFieldWidget ageField $ elDynAttr' "input" (weaken (color ageInput)) (pure unit)
        whenInputIntact ageInput $ el "span" [attr "style" "color: green;"] $ text "mandatory"
        whenInputTouchedIncorrect ageInput $ el "span" [attr "style" "color: red;"] <<< text
    el "div" [] do
      text "Name"
      el "div" [] do
        stringFieldWidget nameField $ elDynAttr' "input" (weaken (color nameInput)) (pure unit)
        whenInputIntact nameInput $ el "span" [attr "style" "color: green;"] $ text "mandatory"
        whenInputTouchedIncorrect nameInput $ el "span" [attr "style" "color: red;"] <<< text
    el "div" [] do
      text "Repeat Name"
      el "div" [] do
        stringFieldWidget repeatedNameField $ elDynAttr' "input" (weaken (color repeatedName)) (pure unit)
        whenInputIntact repeatedName $ el "span" [attr "style" "color: green;"] $ text "mandatory"
        whenInputTouchedIncorrect repeatedName $ el "span" [attr "style" "color: red;"] <<< text
    whenInputCorrect personInput $ \(Person personInput) -> do
      el "div" [] do
        el "h2" [] $ text "Review"
        el "p" [] $ text $ "Age: " <> show personInput.personAge
        el "p" [] $ text $ "Name: " <> show personInput.personName
    pure (Tuple personInput $ \(Tuple nameInput ageInput) -> do
      writeField ageField (Tuple ageInput mempty)
      writeField nameField (Tuple nameInput mempty))

main :: Effect Unit
main = do
  personForm <- mkPersonForm
  runMainWidgetInBody do
    (Tuple personInput populatePerson) <- el "div" [attr "style" "padding: 10px;"] personForm
    el "button" [onClick_ (populatePerson (Tuple "Eryk" "38"))] $ text "Populate"
    whenInputCorrect personInput $ \p -> el "button" [] $ text "Submit"
  pure unit
