module DemoComponentMockup
  ( main
  )
  where

import Prelude

import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Array (head, (!!))
import Data.Foldable (fold, for_)
import Data.Identity (Identity(..))
import Data.Map (Map, empty, lookup, mapMaybeWithKey, singleton, size, toUnfoldable, values)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Specular.Dom.Component (ComponentWrapper, inside, static, swallow, text)
import Specular.Dom.ComponentMDC as MDC
import Specular.Dom.Widget (Widget, runMainWidgetInBody)
import Unsafe.Coerce (unsafeCoerce)


type FieldName = String
type ConstructorName = String

newtype Scenario = Scenario (Map FieldName (Tuple ConstructorName Scenario))

derive instance Newtype Scenario _

instance Show Scenario where
   show (Scenario s) = fold $ values $ mapMaybeWithKey (\fieldName (Tuple constructorName subScenarios) -> Just $ fieldName <> ": " <> constructorName <> (if size (unwrap subScenarios) > 0 then " (" <> show subScenarios <> ")" else "")) s

newtype Scenarios = Scenarios (Map FieldName (Map ConstructorName Scenarios))

extractScenarios :: Scenarios -> Array Scenario
extractScenarios scenarios = let
  a =  (join $ map (map (map extractScenarios >>> sequence) >>> sequence) $ sequence $ unwrap scenarios <#> toUnfoldable :: Array (Map FieldName (Tuple ConstructorName Scenario)))
  in a <#> wrap

derive instance Newtype Scenarios _

instance Semigroup Scenarios where
  append s1 s2 = wrap $ append (unwrap s1) (unwrap s2)

instance Monoid Scenarios where
  mempty = wrap mempty

main :: Effect Unit
main = runMainWidgetInBody $ mockComponent order

newtype Mocking a = Mocking (Tuple Scenarios (ReaderT Scenario Identity a)) 

derive instance Newtype (Mocking a) _

derive instance Functor Mocking

instance Apply Mocking where
  -- apply f a = wrap $ apply (unwrap f) (unwrap a) -- EC: I don't know why it doesn't compile 
  apply f a = Mocking $ let
    (Tuple allscenariosf readerf) = unwrap f 
    (Tuple allscenariosa readera) = unwrap a
    in Tuple (allscenariosf <> allscenariosa) (ReaderT \selectedScenario -> let
      (Identity f) = runReaderT readerf selectedScenario
      (Identity a) = runReaderT readera selectedScenario
      in Identity $ f a)

instance Applicative Mocking where
  pure a = Mocking (Tuple mempty (pure a))
 
mockComponent :: (forall a. ComponentWrapper Mocking a a) -> Widget Unit
mockComponent componentWrapper = do
  let (Tuple allScenarios runWithSelectedScenario) = unwrap $ unwrap componentWrapper
  for_ (extractScenarios allScenarios !! 3) \firstScenario -> do
    let (Identity component) = runReaderT runWithSelectedScenario firstScenario
    unwrap component (pure unit) -- when attempted to read the error will occur 

-- generic lens

mockData :: forall a b s t. String -> a → ComponentWrapper Mocking a b → ComponentWrapper Mocking s t
mockData dataName dataValue = static dataValue >>> swallow

section :: forall a. String -> ComponentWrapper Mocking a a -> ComponentWrapper Mocking a a 
section sectionName = identity


-- generic prism
altSection :: forall a. String -> String -> ComponentWrapper Mocking a a → ComponentWrapper Mocking a a
altSection fieldName constructorName componentWrapper = let 
    (Tuple scenarios runWithSelectedScenario) = unwrap $ unwrap componentWrapper
  in wrap $ wrap $ Tuple (Scenarios $ singleton fieldName (singleton constructorName scenarios)) (ReaderT (\(Scenario scenario) -> case lookup fieldName scenario of
    Nothing -> unsafeThrow "!!!"
    Just (Tuple constructorName' scenario') | constructorName' == constructorName -> runReaderT runWithSelectedScenario scenario'
    _ -> pure mempty  
    ))
  
-- instance (Functor f, Profunctor p, Monoid p) => Monoid (Cayley f p) where
--   dimap f g = wrap <<< map (dimap f g) <<< unwrap

order :: forall a. ComponentWrapper Mocking a a
order =
  (
    (MDC.filledText "Id" # mockData "id" "158712")
    <>
    (
      (text # static "Dine in" # altSection "fulfillment" "dine in")
      <>
      (
        (text # static "Takeaway")
        <>
        (text # mockData "at" "12:45")
      # altSection "fulfillment" "takeaway")
      <>
      (
        (text # static "Delivery")
        <>
        (MDC.filledText "Hour" # mockData "at" "12:10")
        <>
        (
          (
            (text # static "Coordinates")
            <>
            (text # mockData "longitude" "21.28990")
            <>
            (text # mockData "latitude" "50.27898")
          # altSection "place" "coordinates")
          <>
          (
            (text # static "Address" # inside "span" mempty mempty)
            <>
            (MDC.checkbox # mockData "verified" true)
            <>
            (MDC.filledText "City" # mockData "city" "London")
            <>
            (MDC.filledText "Street" # mockData "street" "Abbey Road")
            <>
            (MDC.filledText "Street number" # mockData "street number" "19")
            <>
            (text # static "Clear" # MDC.button)
          # altSection "place" "address")
        # inside "div" mempty mempty # section "to")
      # altSection "fulfillment" "delivery")
    # inside "div" mempty mempty)
    <>
    (MDC.checkbox # mockData "paid" true)
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


itemComponent :: forall a. ComponentWrapper Mocking a a
itemComponent =
  (
    (MDC.filledText "Product" # inside "span" mempty mempty # mockData "product" "Capriciosa")
    <>
    (text # static " x " # inside "span" mempty mempty)
    <>
    (text # inside "span" mempty mempty # mockData "quantity" "2")
  )