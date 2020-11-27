module Specular.Lumiform.LumiformSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (null)
import Effect (Effect)
import Specular.Dom.Element (dynText, el_)
import Specular.Dom.Widget (runMainWidgetInBody)
import Specular.FRP (Dynamic)
import Specular.Lumiform.Lumiform (Form(..), Lumiform, RequiredOrOptional(..), input, lumiform, output, section, unform)

-- DSL expression
data Person = Person {
  firstName ::String,
  lastName :: String
}

main :: Effect Unit
main = do
  runMainWidgetInBody do
    personD <- lumiform ado
      section "First name"
      firstName <- input
      section "Last name"
      lastName <- input
      in Person { firstName, lastName }
    el_ "div" $
      dynText $ ("Hello, " <> _) <<< showPerson <$> personD
    where
      showPerson :: Person -> String
      showPerson (Person r) = r.firstName <> " " <> r.lastName




