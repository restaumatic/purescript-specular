module Specular.Lumiform.LumiformSpec where

import Prelude

import Data.Char.Unicode (isLetter)
import Data.Maybe (maybe)
import Data.String (length, null)
import Data.Traversable (for, intercalate)
import Effect (Effect)
import Specular.Dom.Element (dynText)
import Specular.Dom.Widget (runMainWidgetInBody)
import Specular.Lumiform.Lumiform (assert, char, int, number, section, string, webform)

data Status = Enabled | Disabled

data Person = Person {
  firstName ::String,
  lastName :: String,
  middleInitial :: Char,
  addressLines :: Array String,
  age :: Int,
  height :: Number
}
-- TODO:
-- make WebForm a MonadRec
-- Form and WebForm Form interpreter in separate modules (Form should be lightweight)
-- value options
-- web form interpreter for Boolean and enums
main :: Effect Unit
main = do
  runMainWidgetInBody do
    -- here's an introduction of lumi-like form
    person <- webform ado
      section "Person data"
      firstName <- string "First name" [
        assert (not <<< null) "must be not empty",
        assert ((_ < 3) <<< length) "must be shorter than 3 characters"]
      lastName <- string "Last name" [
        assert ((_ >= 3) <<< length) "must be at least 3 characters long"]
      middleInitial <- char "Middle initial" [
        assert isLetter "must be a letter"]
      addressLines <- for [1, 2, 3] $ \addressLineNumber -> string ("Address line " <> show addressLineNumber) mempty
      age <- int "Age" [
        assert (_ >= 0) "must not be negative"]
      height <- number "Height" [
        assert (_ > 0.0) "must be positive"]
      in Person { firstName, lastName, middleInitial, addressLines, age, height}
    -- here we continue regular usage of Specular
    dynText $ ("Hello, " <> _) <<< maybe "uknown" showPerson <$> person
    where
      showPerson :: Person -> String
      showPerson (Person r) = r.firstName <> " " <> r.lastName <> ", " <> intercalate " " r.addressLines
