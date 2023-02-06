module DemoComponent
  ( main
  )
  where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (only)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap, lcmap, rmap)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Console (log)
import Specular.Dom.Component (Component, aff, controlled, controller, eff, eff_, inside, nth, onChange, prismEq, propEq, react, react_, renderComponent, spawn', static, text, whenControl, withControl)
import Specular.Dom.ComponentMDC as MDC
import Specular.Dom.Widget (runMainWidgetInBody)
import Type.Proxy (Proxy(..))

type Order =
  { id :: String
  , fulfillment :: Fulfillment
  , items :: Array Item
  , paid :: Boolean
  , customer :: String
  }

data Fulfillment
  = DineIn
  | Takeaway
    { at :: Hour}
  | Delivery
    { to :: Place
    , at :: Hour
    }
derive instance Generic Fulfillment _
derive instance Eq Fulfillment
instance Show Fulfillment where
  show = genericShow

type Item =
  { product :: Product
  , qty :: Int
  , addition :: Maybe Addition
  }

type Hour = String

data Place
  = Coords
    { long :: String
    , lat :: String
    }
  | Address
    { city :: String
    , street :: String
    , streetNumber :: String}

derive instance Generic Place _
derive instance Eq Place

instance Show Place where
  show = genericShow

type Product = String

type Addition = String

-- optics
id = propEq (Proxy :: Proxy "id")
items = propEq (Proxy :: Proxy "items")
delivery = prismEq Delivery $ case _ of
  Delivery d -> Just d
  _ -> Nothing
takeaway = prismEq Takeaway $ case _ of
  Takeaway t -> Just t
  _ -> Nothing
dineIn = only DineIn
coords = prismEq Coords $ case _ of
  Coords c -> Just c
  _ -> Nothing
address = prismEq Address $ case _ of
  Address a -> Just a
  _ -> Nothing
long = propEq (Proxy :: Proxy "long")
lat = propEq (Proxy :: Proxy "lat")
city = propEq (Proxy :: Proxy "city")
street = propEq (Proxy :: Proxy "street")
streetNumber = propEq (Proxy :: Proxy "streetNumber")
at = propEq (Proxy :: Proxy "at")
to = propEq (Proxy :: Proxy "to")
product = propEq (Proxy :: Proxy "product")
qty = propEq (Proxy :: Proxy "qty")
fulfillment = propEq (Proxy :: Proxy "fulfillment")
paid = propEq (Proxy :: Proxy "paid")
paid' = prop (Proxy :: Proxy "paid")
customer = propEq (Proxy :: Proxy "customer")

-- data ShowMode = Capitals | Verbatim

main :: Effect Unit
main = mempty
-- main = runMainWidgetInBody do
--   -- Data model - not a view model
--   let initialOrder =
--         { id: "178"
--         , fulfillment: Delivery
--           { to: Address
--             { city: "London"
--             , street: "Abbey Road"
--             , streetNumber: "13"
--             }
--           , at: "12:15"
--           }
--         , items:
--           [ { product: "Cappriciosa"
--             , qty: 2
--             , addition: Just "garlic sauce"}
--           , { product: "Siciliana"
--             , qty: 1
--             , addition: Nothing}
--           ]
--         , paid: true
--         , customer: "John Doe"
--         }
--   -- View
--   order # renderComponent initialOrder

-- order :: Component Order Order
-- order =
--   (
--     (MDC.filledText "Id" # id)
--     <>
--     (text # static "Generate" # MDC.button #id # react_ (aff (const (delay (Milliseconds 3000.0) *> pure "13"))))
--     <>
--     (text # static "Generate" # MDC.button # onChange
--       (text)
--     #id )
--     <>
--     (
--       (text # static "Dine-in" # only DineIn)
--       <>
--       (
--         (text # static "Takeaway")
--         <>
--         (text # at)
--       # takeaway)
--       <>
--       (
--         (text # static "Delivery")
--         <>
--         (text # inside "div" mempty mempty # at)
--         <>
--         (
--           (
--             (text # static "Coordinates")
--             <>
--             (text # long)
--             <>
--             (text # lat)
--           # coords)
--           <>
--           (
--             (text # static "Address" # inside "span" mempty mempty)
--             <>
--             (MDC.checkbox # dimap (case _ of
--               Verbatim -> false
--               Capitals -> true) (if _ then Capitals else Verbatim) # controller)
--             <>
--             (MDC.filledText "City" # city # controlled)
--             <>
--             (MDC.filledText "Street" # street # controlled)
--             <>
--             (MDC.filledText "Street number" # streetNumber # controlled)
--             <>
--             (text # static "Clear" # MDC.button # react (eff (const $ pure $ { city: "", street: "", streetNumber: ""})) # controlled)
--           # withControl Capitals # address)
--         # inside "div" mempty mempty # to)
--       # delivery)
--     # inside "div" mempty mempty # fulfillment)
--     <>
--     (MDC.checkbox # paid)
--     <>
--     ( 
--       (text # static "Customer" # inside "span" mempty mempty)
--       <>
--       (
--         (
--           (text # static "Show" # MDC.button # rmap (const true))
--           <>
--           (text # static "Hide" # MDC.button # rmap (const false))
--         )
--       # controller)
--       <>
--       (text # static "Peek" # MDC.button # onChange 
--         (text)
--        # controlled)
--       <>
--       (MDC.filledText "Customer" # whenControl identity)

--     # inside "div" mempty mempty # withControl true # customer)
--     <>
--     (
--       (itemComponent # inside "li" mempty mempty # nth 0)
--       <>
--       (itemComponent # inside "li" mempty mempty # nth 1)
--       <>
--       (itemComponent # inside "li" mempty mempty # nth 2)
--       <>
--       (itemComponent # inside "li" mempty mempty # nth 3)
--       <>
--       (itemComponent # inside "li" mempty mempty # nth 4)
--     # inside "ol" mempty mempty # items)
--     <>
--     (text # static "Submit" # MDC.button # spawn' 
--       (
--         (
--           (
--             (text # static "Order is not paid. Submit anyway?")
--             <>
--             (text # static "Yes" # MDC.button) 
--           # only false # paid')
--         )
--       )
--     # react_ (eff_ (\b -> log (show b))))
--     <>
--     (text # lcmap show # inside "p" mempty mempty)
--   # inside "div" mempty mempty)


-- itemComponent =
--   (
--     (MDC.filledText "Product" # inside "span" mempty mempty # product)
--     <>
--     (text # static " x " # inside "span" mempty mempty)
--     <>
--     (text # inside "span" mempty mempty # lcmap show # qty)
--   )