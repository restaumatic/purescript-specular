module DemoPWidget
  ( button
  , main
  )
  where

import Prelude

import Control.Monad.Cleanup (runCleanupT)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens, only)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap, lcmap, rmap)
import Data.Show.Generic (genericShow)
import Data.String (length)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, mkEffectFn2, runEffectFn2)
import Prim.Row as Row
import Specular.Dom.Browser (Node, (:=))
import Specular.Dom.Element (class_)
import Specular.Dom.PWidget (PWidget(..), checkbox, controlled, controller, inside, onClick, prismEq, propEq, static, text, textInput, whenControl, withControl, withRef)
import Specular.Dom.Widget (Widget, runMainWidgetInBody)
import Specular.FRP (never, uniqDyn)
import Specular.Ref (newRef)
import Type.Proxy (Proxy(..))

foreign import data ComponentClass :: Type
foreign import data Component :: Type

foreign import material
  :: { textField :: { "MDCTextField" :: ComponentClass }
     , ripple :: { "MDCRipple" :: ComponentClass }
     , drawer :: { "MDCDrawer" :: ComponentClass }
     , tabBar :: { "MDCTabBar" :: ComponentClass }
     , dialog :: { "MDCDialog" :: ComponentClass }
     , snackbar :: { "MDCSnackbar" :: ComponentClass }
     , radio :: { "MDCRadio" :: ComponentClass }
     , chips :: { "MDCChip" :: ComponentClass }
     , select :: { "MDCSelect" :: ComponentClass }
     , list :: { "MDCList" :: ComponentClass }
     , checkbox :: { "MDCCheckbox" :: ComponentClass }
     }


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
    (filledText "Id" # id)
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
            -- (checkbox' # dimap (case _ of
            --   Verbatim -> false
            --   Capitals -> true) (if _ then Capitals else Verbatim) # controller)
            -- <>
            (filledText "City" # city # controlled)
            <>
            (filledText "Street" # street # controlled)
            <>
            (filledText "Street number" # streetNumber # controlled)
          # withControl Capitals # address)
        # inside "div" mempty mempty # to)
      # delivery)
    # inside "div" mempty mempty # fulfillment)
    <>
    (checkbox' # payed)
    <>
    ( 
      (text # static "Customer" # inside "span" mempty mempty)
      <>
      (
        (
          (text # static "Show" # button # rmap (const true))
          <>
          (text # static "Hide" # button # rmap (const false))
        )
      # controller)
      <>
      (filledText "Customer" # whenControl identity)
    # inside "div" mempty mempty # withControl true # customer)
  # inside "div" mempty mempty # withRef orderRef)

  text # lcmap show # inside "p" mempty mempty # withRef orderRef


-- MDC components


foreign import _new :: EffectFn2 ComponentClass Node Component



mdcWith :: ComponentClass -> Node -> (Component -> Node -> Effect Unit) -> Effect Unit
mdcWith class_ node init = do
  component <- new class_ node
  pure unit
  -- Tuple _ cleanup <- (map fst <<< runCleanupT) $ init component node
  -- pushDelayed cleanups cleanup
  where
    new :: ComponentClass -> Node -> Effect Component
    new cls node = liftEffect $ runEffectFn2 _new cls node


button :: forall a. PWidget a Unit -> PWidget a Unit
button text =
  inside "button" (const $ "class" := "mdc-button mdc-button--raised foo-button") ((\_ node -> (liftEffect $ mdcWith material.ripple."MDCRipple" node mempty) *> pure never) <> onClick unit) $
    (inside "div" (const $ "class" := "mdc-button__ripple") mempty mempty)
    <>
    (inside "span" (const $ "class" := "mdc-button__label") mempty text)

filledText :: String -> PWidget String String
filledText hintText =
  inside "label" (const $ "class" := "mdc-text-field mdc-text-field--filled") (\_ node -> (liftEffect $ mdcWith material.textField."MDCTextField" node mempty) *> pure never) $
    (inside "span" (const $ "class" := "mdc-text-field__ripple") mempty mempty)
    <>
    (inside "span" (const $ "class" := "mdc-floating-label" <> "id" := "my-label-id") mempty (text # static hintText))
    <>
    (textInput (const $ "class" := "mdc-text-field__input" <> "type" := "text" <> "aria-labelledby" := "my-label-id"))
    <>
    (inside "span" (const $ "class" := "mdc-line-ripple") mempty mempty)


checkbox' :: PWidget Boolean Boolean
checkbox' = inside "div" (const $ "class" := "mdc-touch-target-wrapper") (\_ node -> (liftEffect $ mdcWith material.checkbox."MDCCheckbox" node mempty) *> pure never) $
  inside "div" (const $ "class" := "mdc-checkbox mdc-checkbox--touch") mempty $
    (checkbox (const $ "class" := "mdc-checkbox__native-control"))
    <>
    (inside "div" (const $ "class":= "mdc-checkbox__background") mempty $
      inside "svg" (const $ "class" := "mdc-checkbox__checkmark" <> "viewBox" := "0 0 24 24") mempty $
        (inside "path" (const $ "class" := "mdc-checkbox__checkmark-path" <> "fill" := "none" <> "d" := "M1.73,12.91 8.1,19.28 22.79,4.59") mempty mempty)
        <>
        (inside "div" (const $ "class" := "mdc-checkbox__mixedmark") mempty mempty)
    )
    <>
    (inside "div" (const $ "class" := "mdc-checkbox__ripple") mempty mempty)
