module DemoPWidget
  ( main
  )
  where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (only, prism')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Show.Generic (genericShow)
import Data.String (length)
import Effect (Effect)
import Specular.Dom.Browser ((:=))
import Specular.Dom.Widget (runMainWidgetInBody)
import Specular.Ref (newRef)
import Type.Proxy (Proxy(..))
import Specular.Dom.PWidget (checkbox, controller, inside, onClick, static, text, textInput, whenControl, withControl, withRef)

type Order =
  { id :: String
  , fulfillment :: Fulfillment
  , items :: Array Item
  , payed :: Boolean
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
id = prop (Proxy :: Proxy "id")
items = prop (Proxy :: Proxy "items")
delivery = prism' Delivery $ case _ of
  Delivery d -> Just d
  _ -> Nothing
takeaway = prism' Takeaway $ case _ of
  Takeaway t -> Just t
  _ -> Nothing
coords = prism' Coords $ case _ of
  Coords c -> Just c
  _ -> Nothing
address = prism' Address $ case _ of
  Address a -> Just a
  _ -> Nothing
long = prop (Proxy :: Proxy "long")
lat = prop (Proxy :: Proxy "lat")
city = prop (Proxy :: Proxy "city")
street = prop (Proxy :: Proxy "street")
streetNumber = prop (Proxy :: Proxy "streetNumber")
at = prop (Proxy :: Proxy "at")
to = prop (Proxy :: Proxy "to")
product = prop (Proxy :: Proxy "product")
qty = prop (Proxy :: Proxy "qty")
fulfillment = prop (Proxy :: Proxy "fulfillment")
payed = prop (Proxy :: Proxy "payed")
customer = prop (Proxy :: Proxy "customer")

main :: Effect Unit
main = runMainWidgetInBody do
  -- Data model - not a view model
  orderRef <- newRef
    { id: "7"
    , fulfillment: Delivery
      { to: Address
        { city: "London"
        , street: "Abbey Road"
        , streetNumber: "13"
        }
      , at: "12:15"
      }
    , items:
      [ { product: "Cappriciosa"
        , qty: 2
        , addition: Just "garlic sauce"}
      , { product: "Siciliana"
        , qty: 1
        , addition: Nothing}
      ]
    , payed: true
    , customer: "John Doe"
    }
  -- View
  (
    (textInput (\i -> if length i > 1 then "class" := "red" else mempty <> "placeholder" := "Id") # id)
    <>
    (
      (text # static "Dine-in" # only DineIn)
      <>
      (
        (text # static "Takeaway")
        <>
        (text # at)
      # takeaway)
      <>
      (
        (text # static "Delivery")
        <>
        (text # inside "div" mempty mempty # at)
        <>
        (
          (
            (text # static "Coordinates")
            <>
            (text # long)
            <>
            (text # lat)
          # coords)
          <>
          (
            (text # static "Address" # inside "span" mempty mempty)
            <>
            (textInput mempty # inside "span" mempty mempty # city)
            <>
            (textInput mempty # inside "span" mempty mempty # street)
            <>
            (textInput mempty # inside "span" mempty mempty # streetNumber)
          # address)
        # inside "div" mempty mempty # to)
      # delivery)
    # inside "div" mempty mempty # fulfillment)
    <>
    (checkbox mempty # payed)
    <>
    ( 
      (text # static "Customer" # inside "span" mempty mempty)
      <>
      (
        (
          (text # static "Show" # inside "span" (\enabled -> if enabled then "class" := "red" else mempty) (onClick true))
          <>
          (text # static "Hide" # inside "span" (\enabled -> if not enabled then "class" := "red" else mempty) (onClick false))
        )
      # controller)
      <>
      (textInput mempty # inside "p" mempty mempty # whenControl identity)
    # inside "div" mempty mempty # withControl true # customer)
  # inside "div" mempty mempty # withRef orderRef)

  text # lcmap show # inside "p" mempty mempty # withRef orderRef
