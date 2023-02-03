module DemoComponentMockup
  ( main
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Lens (prism)
import Data.Newtype (unwrap)
import Effect (Effect)
import Specular.Dom.Component (Component, inside, static, swallow, text)
import Specular.Dom.ComponentMDC as MDC
import Specular.Dom.Widget (Widget, runMainWidgetInBody)
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = runMainWidgetInBody $ mockComponent order

mockComponent :: forall a b. Component a b -> Widget Unit
mockComponent component = do
  let nonExistingDynamic = pure (unsafeCoerce unit) -- when attempted to read the error will occur 
  void $ unwrap component nonExistingDynamic

-- generic lens
mockData :: forall a b s t. a → Component a b → Component s t
mockData a = static a >>> swallow

-- generic prism
mockShow :: forall a b s t. Component a b → Component s t
mockShow = prism unsafeCoerce (Right <<< unsafeCoerce)

-- generic prism
mockHide :: forall a b s t. Component a b → Component s t
mockHide = prism unsafeCoerce (Left <<< unsafeCoerce)

order :: forall a b. Component a a
order =
  (
    (MDC.filledText "Id" # mockData "100")
    <>
    (text # static "Generate" # MDC.button)
    <>
    (
      (text # static "Dine-in" # mockHide)
      <>
      (
        (text # static "Takeaway")
        <>
        (text # mockData "12:45")
      # mockHide)
      <>
      (
        (text # static "Delivery")
        <>
        (text # inside "div" mempty mempty # mockData "12:45")
        <>
        (
          (
            (text # static "Coordinates")
            <>
            (text # mockData "21.28990")
            <>
            (text # mockData "50.27898")
          # mockHide)
          <>
          (
            (text # static "Address" # inside "span" mempty mempty)
            <>
            (MDC.checkbox # mockData true)
            <>
            (MDC.filledText "City" # mockData "London")
            <>
            (MDC.filledText "Street" # mockData "Abbey Road")
            <>
            (MDC.filledText "Street number" # mockData "19")
            <>
            (text # static "Clear" # MDC.button)
          # mockShow)
        # inside "div" mempty mempty)
      # mockShow)
    # inside "div" mempty mempty)
    <>
    (MDC.checkbox # mockData true)
    <>
    ( 
      (text # static "Customer" # inside "span" mempty mempty)
      -- <>
      -- (
      --   (
      --     (text # static "Show" # MDC.button)
      --     <>
      --     (text # static "Hide" # MDC.button)
      --   )
      -- # controller)
    # inside "div" mempty mempty)
    <>
    (
      (itemComponent # inside "li" mempty mempty)
      <>
      (itemComponent # inside "li" mempty mempty)
    # inside "ol" mempty mempty)
  # inside "div" mempty mempty)


itemComponent :: forall a a. Component a a
itemComponent =
  (
    (MDC.filledText "Product" # inside "span" mempty mempty # mockData "Capriciosa")
    <>
    (text # static " x " # inside "span" mempty mempty)
    <>
    (text # inside "span" mempty mempty # mockData "2")
  )