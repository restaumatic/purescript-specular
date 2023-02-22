module DemoComponentMockup
  ( main
  )
  where

import Prelude

import BuilderSpec (newDynamic)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Array (head, length, (!!))
import Data.Foldable (fold, for_, intercalate)
import Data.Identity (Identity(..))
import Data.Int (fromString)
import Data.Map (Map, lookup, mapMaybeWithKey, singleton, size, toUnfoldable, values)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (joinWith)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception.Unsafe (unsafeThrow)
import Specular.Dom.Component (ComponentWrapper, component, inside, static, swallow, text)
import Specular.Dom.ComponentMDC as MDC
import Specular.Dom.Widget (Widget, runMainWidgetInBody)
import Specular.FRP (readDynamic, subscribeDyn_, whenJustD, withDynamic_)
import Specular.Ref (modify, newRef, read, value, write)

-- foreign import locationHash :: Effect String

foreign import onHash :: (String -> Effect Unit) -> Effect Unit

foreign import setHash :: String -> Effect Unit

foreign import setTitle :: String -> Effect Unit

foreign import onKeyDown :: (String -> Effect Unit) -> Effect Unit


type FieldName = String
type ConstructorName = String

newtype Scenario = Scenario (Map FieldName (Tuple ConstructorName Scenario))

derive instance Newtype Scenario _

instance Show Scenario where
   show (Scenario s) = intercalate " | " $ values $ mapMaybeWithKey (\fieldName (Tuple constructorName subScenarios) -> Just $ fieldName <> ": " <> constructorName <> (if size (unwrap subScenarios) > 0 then " (" <> show subScenarios <> ")" else "")) s

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
main = mockComponent order

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
 
mockComponent :: (forall a. ComponentWrapper Mocking a a) -> Effect Unit
mockComponent componentWrapper = do
  let (Tuple allScenarios' runWithSelectedScenario) = unwrap $ unwrap componentWrapper
  let allScenarios = extractScenarios allScenarios'
  let noOfScenarios = length allScenarios 
  log (show noOfScenarios <> " scenario(s): " <> show allScenarios)
  runMainWidgetInBody $ do
    mScenarioNoRef <- newRef Nothing
    liftEffect $ onHash \hash -> do
      let mScenarioNo = fromString hash >>= (\n -> if n < 0 || n >= noOfScenarios then Nothing else Just n) 
      write mScenarioNoRef mScenarioNo
    liftEffect $ onKeyDown \keyCode -> do
      mn <- read mScenarioNoRef
      case keyCode of
        -- Left
        "ArrowLeft" -> (setHash <<< show <<< maybe 0 (\n -> if n == 0 then n else n - 1)) mn
        -- Right
        "ArrowRight" -> (setHash <<< show <<< maybe 0 (\n -> if n == noOfScenarios - 1 then n else n + 1)) mn
        -- Up
        "ArrowUp" -> (setHash <<< show) (noOfScenarios - 1)
        -- Down
        "ArrowDown" -> (setHash <<< show) 0
        _ -> pure unit
    whenJustD (value mScenarioNoRef <#> (_ >>= \n -> allScenarios !! n)) $ flip withDynamic_ \scenario -> do
      liftEffect $ setTitle $ show scenario
      let (Identity component) = runReaderT runWithSelectedScenario scenario
      void $ unwrap component (pure {path: [], value: unit}) -- when attempted to read the error will occur
  pure unit


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
  
enabledWhen :: forall a. String -> String -> ComponentWrapper Mocking Boolean Boolean → ComponentWrapper Mocking a a
enabledWhen fieldName constructorName componentWrapper = let 
    (Tuple scenariosForTrue runWithSelectedScenarioForTrue) = unwrap $ unwrap (componentWrapper # static true # swallow)
    (Tuple scenariosForFalse runWithSelectedScenarioForFalse) = unwrap $ unwrap (componentWrapper # static false # swallow)
  in wrap $ wrap $ Tuple (Scenarios $ singleton fieldName (singleton "true" mempty <> singleton "false" mempty)) (ReaderT (\(Scenario scenario) -> case lookup fieldName scenario of
    Nothing -> unsafeThrow "!!!"
    Just (Tuple constructorName' scenario') -> runReaderT (if constructorName' == constructorName then runWithSelectedScenarioForTrue else runWithSelectedScenarioForFalse) scenario'
    ))

-- instance (Functor f, Profunctor p, Monoid p) => Monoid (Cayley f p) where
--   dimap f g = wrap <<< map (dimap f g) <<< unwrap

order :: forall a. ComponentWrapper Mocking a a
order =
  component
    (MDC.filledText "Id" # mockData "id" "42375293")
    (component
      (text # static "Dine in" # altSection "fulfillment" "dine in")
      (altSection "fulfillment" "takeaway" $ component
        (text # static "Takeaway")
        (text # mockData "at" "12:45")
      )
      (component
        (text # static "Delivery")
        (MDC.filledText "Hour" # mockData "at" "12:10")
        (component
          (component
            (text # static "Coordinates")
            (text # mockData "longitude" "21.28990")
            (text # mockData "latitude" "50.27898")
          # altSection "place" "coordinates")
          (component
            (text # static "Address" # inside "span" mempty mempty)
            (MDC.checkbox # mockData "verified" true)
            (MDC.filledText "City" # mockData "city" "London")
            (MDC.filledText "Street" # mockData "street" "Abbey Road")
            (MDC.filledText "Street number" # mockData "street number" "19")
            (text # static "Clear" # MDC.button)
          # altSection "place" "address")
        # inside "div" mempty mempty # section "to")
      # altSection "fulfillment" "delivery")
    # inside "div" mempty mempty)
    (MDC.checkbox # enabledWhen "paid" "true")
    (MDC.filledText "Payment method" # mockData "payment method" "Cash" # altSection "paid" "true")
    (text # static "Customer" # inside "span" mempty mempty # inside "div" mempty mempty)
    (component
      (itemComponent # inside "li" mempty mempty)
      (itemComponent # inside "li" mempty mempty)
    # inside "ol" mempty mempty)
  # inside "div" mempty mempty


itemComponent :: forall a. ComponentWrapper Mocking a a
itemComponent =
  (component
    (MDC.filledText "Product" # inside "span" mempty mempty # mockData "product" "Capriciosa")
    (text # static " x " # inside "span" mempty mempty)
    (text # inside "span" mempty mempty # mockData "quantity" "2")
  )
