module DemoPWidget
  ( main
  )
  where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (only)
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap, lcmap, rmap)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Specular.Dom.PWidget (bar, controlled, controller, foo, inside, prismEq, propEq, static, text, whenControl, withControl, withRef)
import Specular.Dom.PWidgetMDC as MDC
import Specular.Dom.Widget (runMainWidgetInBody)
import Specular.Ref (newRef)
import Type.Proxy (Proxy(..))

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
id = propEq (Proxy :: Proxy "id")
items = propEq (Proxy :: Proxy "items")
delivery = prismEq Delivery $ case _ of
  Delivery d -> Just d
  _ -> Nothing
takeaway = prismEq Takeaway $ case _ of
  Takeaway t -> Just t
  _ -> Nothing
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
payed = propEq (Proxy :: Proxy "payed")
customer = propEq (Proxy :: Proxy "customer")

data ShowMode = Capitals | Verbatim

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
    (MDC.filledText "Id" # id)
    <>
    (text # static "Generate" # MDC.button # bar (const (delay (Milliseconds 3000.0) *> pure "13")) # id)
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
            (MDC.checkbox # dimap (case _ of
              Verbatim -> false
              Capitals -> true) (if _ then Capitals else Verbatim) # controller)
            <>
            (MDC.filledText "City" # city # controlled)
            <>
            (MDC.filledText "Street" # street # controlled)
            <>
            (MDC.filledText "Street number" # streetNumber # controlled)
            <>
            (text # static "Clear" # MDC.button # bar (const $ pure $ { city: "", street: "", streetNumber: ""}) # controlled)
          # withControl Capitals # address)
        # inside "div" mempty mempty # to)
      # delivery)
    # inside "div" mempty mempty # fulfillment)
    <>
    (MDC.checkbox # payed)
    <>
    ( 
      (text # static "Customer" # inside "span" mempty mempty)
      <>
      (
        (
          (text # static "Show" # MDC.button # rmap (const true))
          <>
          (text # static "Hide" # MDC.button # rmap (const false))
        )
      # controller)
      <>
      (MDC.filledText "Customer" # whenControl identity)
    # inside "div" mempty mempty # withControl true # customer)
    <>
    ( text # static "Submit" # MDC.button # foo (show >>> log >>> liftEffect))
  # inside "div" mempty mempty # withRef orderRef)

  text # lcmap show # inside "p" mempty mempty # withRef orderRef