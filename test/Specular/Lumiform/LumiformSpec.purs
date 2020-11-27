module Specular.Lumiform.LumiformSpec where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.String (length, null)
import Data.Traversable (for, intercalate)
import Effect (Effect)
import Specular.Dom.Element (dynText)
import Specular.Dom.Widget (runMainWidgetInBody)
import Specular.Lumiform.Lumiform (textInput, form, section)

data Person = Person {
  firstName ::String,
  lastName :: String,
  addressLines :: Array String
}

main :: Effect Unit
main = do
  runMainWidgetInBody do
    -- here's an introduction of lumi-like form
    person <- form ado
      section "Person data"
      firstName <- textInput "First name" (\s -> if null s then Left "First name must not be empty" else Right s)
      lastName <- textInput "Last name" (\s -> if length s < 3 then Left "Last name must be at least 3 characters" else Right s)
      addressLines <- for [1, 2, 3] $ \addressLineNumber -> textInput ("Address line " <> show addressLineNumber) Right
      in Person { firstName, lastName, addressLines }
    -- here we continue regular usage of Specular
    dynText $ ("Hello, " <> _) <<< maybe "uknown" showPerson <$> person
    where
      showPerson :: Person -> String
      showPerson (Person r) = r.firstName <> " " <> r.lastName <> ", " <> intercalate " " r.addressLines
