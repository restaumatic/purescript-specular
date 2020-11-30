module Specular.Lumiform.LumiformSpec where

import Prelude

import Data.Char.Unicode (isLetter)
import Data.Maybe (maybe)
import Data.String (length, null)
import Data.Traversable (for, intercalate)
import Effect (Effect)
import Specular.Dom.Element (dynText)
import Specular.Dom.Widget (runMainWidgetInBody)
import Specular.Lumiform.Form (assert, char, int, number, section, string, webform)

--
data Person = Person {
  firstName ::String,
  lastName :: String,
  middleInitial :: Char,
  addressLines :: Array String,
  age :: Int,
  height :: Number
}

-- TODO:
-- DONE make WebForm a MonadRec
-- Form and WebForm Form interpreter in separate modules (Form should be lightweight)
-- value options
-- web form interpreter for Boolean and enums
-- Form not fulfilling law m1 <*> m2 = m1 >>= (x1 -> m2 >>= (x2 -> return (x1 x2)))
main :: Effect Unit
main = do
  runMainWidgetInBody do
    -- here's an introduction of lumi-like form
    person <- webform do
      section "Person data"
      firstName <- string "First name" [
        assert (not <<< null) "must be not empty",
        assert ((_ < 10) <<< length) "must be shorter than 10 characters"]
      lastName <- string "Last name" [
        assert ((_ > length firstName) <<< length) "must be longer than first name"]
      middleInitial <- char "Middle initial" [
        assert isLetter "must be a letter"]
      addressLines <- for [1, 2, 3] $ \addressLineNumber -> string ("Address line " <> show addressLineNumber) mempty
      age <- int "Age" [
        assert (_ >= 0) "must not be negative"]
      height <- number "Height" [
        assert (_ > 0.0) "must be positive"]
      pure $ Person { firstName, lastName, middleInitial, addressLines, age, height}
    -- here we continue regular usage of Specular
    dynText $ ("Hello, " <> _) <<< maybe "uknown" showPerson <$> person
    where
      showPerson :: Person -> String
      showPerson (Person r) = r.firstName <> " " <> r.lastName <> ", " <> intercalate " " r.addressLines
