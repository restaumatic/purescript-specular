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


data Person = Person {
  personAge :: Int,
  personName :: String
}

mkPersonForm :: Effect (Form Dynamic Person (Tuple String String))
mkPersonForm = do
  ageField <- field
  nameField <- field
  repeatedNameField <- field

  let
    age = do
      text <- fieldInput ageField
      when (null text) $ throwError "must not be empty"
      int <- except $ case Int.fromString text of
        Nothing -> Left "Not an Int"
        Just i -> Right i
      positiveInt <- except $ case int of
        i | i > 0 -> Right i
        otherwise -> Left "must be positive"
      pure positiveInt
    color :: forall a . Input Dynamic a -> Dynamic Attrs
    color input = runInput input <#> (\x -> case x of
      Tuple (Just (Right _)) _ -> "style" := "border: 1px solid green;"
      Tuple (Just (Left _)) Touched -> "style" := "border: 1px solid red;"
      _ -> "style" := "border: 1px solid black;")
    name = do
      text <- fieldInput nameField
      when (null text) $ throwError "must not be empty"
      validAge <- validOrUndetermined age
      when (validAge < 10 && length text > 10) $ throwError "Too long name for such a young child"
      pure text
    repeatedName = do
      validName <- validOrUndetermined name
      text <- fieldInput repeatedNameField
      when (validName /= text) $ throwError "Name mismatch"
      pure text
    personInput = (\a n -> Person { personAge: a, personName: n}) <$> age <*> repeatedName

  pure do
    el "h1" [] $ text "Person Form"
    whenInputIntact personInput $ el "span" [attr "style" "color: green;"] $ text "Please fill in below"
    el "div" [] do
      text "Age"
      el "div" [] do
        stringFieldWidget ageField $ elDynAttr' "input" (weaken (color age)) (pure unit)
        whenInputIntact (fieldInput ageField) $ el "span" [attr "style" "color: green;"] $ text "mandatory"
        whenInputTouchedInvalid age $ el "span" [attr "style" "color: red;"] <<< text
    el "div" [] do
      text "Name"
      el "div" [] do
        stringFieldWidget nameField $ elDynAttr' "input" (weaken (color name)) (pure unit)
        whenInputIntact (fieldInput nameField) $ el "span" [attr "style" "color: green;"] $ text "mandatory"
        whenInputTouchedInvalid name $ el "span" [attr "style" "color: red;"] <<< text
    el "div" [] do
      text "Repeat Name"
      el "div" [] do
        stringFieldWidget repeatedNameField $ elDynAttr' "input" (weaken (color repeatedName)) (pure unit)
        whenInputIntact (fieldInput repeatedNameField) $ el "span" [attr "style" "color: green;"] $ text "mandatory"
        whenInputTouchedInvalid repeatedName $ el "span" [attr "style" "color: red;"] <<< text
    whenInputValid personInput $ \(Person personInput) -> do
      el "div" [] do
        el "h2" [] $ text "Review"
        el "p" [] $ text $ "Age: " <> show personInput.personAge
        el "p" [] $ text $ "Name: " <> show personInput.personName
    pure (Tuple personInput $ \(Tuple name age) -> do
      writeField ageField (Tuple age mempty)
      writeField nameField (Tuple name mempty))

main :: Effect Unit
main = do
  personForm <- mkPersonForm
  runMainWidgetInBody do
    (Tuple personInput populatePerson) <- el "div" [attr "style" "padding: 10px;"] personForm
    el "button" [onClick_ (populatePerson (Tuple "Eryk" "38"))] $ text "Populate"
    whenInputValid personInput $ \p -> el "button" [] $ text "Submit"
  pure unit
