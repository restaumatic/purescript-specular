module Specular.Form.Form where

import Prelude

import Control.Applicative.Free (FreeAp, foldFreeAp, liftFreeAp)
import Data.Array (uncons, mapMaybe)
import Data.Either (Either(..), either)
import Data.Functor.Contravariant ((>$<))
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (NonEmpty(..), head)
import Data.Number as Num
import Data.String.CodeUnits (toChar)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect.Exception.Unsafe (unsafeThrow)
import Specular.Callback (attachEvent)
import Specular.Dom.Builder.Class (domEventWithSample, elDynAttr')
import Specular.Dom.Element (dynText, el, text)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Input (getTextInputValue, setTextInputValue)
import Specular.FRP (Dynamic, subscribeDyn_, withDynamic_)
import Specular.Ref (Ref(..), new)

-- DSL
type Error = String
type Constraint a = (a -> Maybe Error)
type Constraints a = Array (Constraint a)

assert :: forall a . (a -> Boolean) -> Error -> Constraint a
assert ass error a = if not (ass a) then Just error else Nothing

cmapConstraints :: forall a b . (b -> a) -> Constraints a -> Constraints b
cmapConstraints f cs = (\c -> c <<< f) <$> cs
cMaybeMapConstraints :: forall a b . (b -> Maybe a) -> Error -> Constraints a -> Constraints b
cMaybeMapConstraints f err = map $ \ca b -> case f b of
  Nothing -> Just err
  Just a -> ca a

data FormF a =
  -- metadata
  Section String a
  -- base type inputs
  | StringInput String (Constraints String) (String -> a)
  | BooleanInput String (Constraints Boolean) (Boolean -> a)
  | IntInput String (Constraints Int) (Int -> a)
  | NumberInput String (Constraints Number) (Number -> a)
  | CharInput String (Constraints Char) (Char -> a)
  -- BoundedEnum a => Show a => Read a
  | EnumSelect String (Array String) (Constraints String) (String -> a)
  | EnumMultiSelect String (Array String) (Constraints (Array String)) (Array String -> a)

instance functorLumiformF :: Functor FormF where
  map f (Section s a) = Section s (f a)
  map f (StringInput l v g) = StringInput l v (f <<< g)
  map f (BooleanInput l v g) = BooleanInput l v (f <<< g)
  map f (IntInput l v g) = IntInput l v (f <<< g)
  map f (NumberInput l v g) = NumberInput l v (f <<< g)
  map f (CharInput l v g) = CharInput l v (f <<< g)
  map f (EnumSelect l o v g) = EnumSelect l o v (f <<< g)
  map f (EnumMultiSelect l o v g) = EnumMultiSelect l o v (f <<< g)

type Form = FreeAp FormF

section :: String -> Form Unit
section s = liftFreeAp $ Section s unit

string :: String -> (Constraints String) -> Form String
string label constraints = liftFreeAp $ StringInput label constraints identity

boolean :: String -> (Constraints Boolean) -> Form Boolean
boolean label constraints = liftFreeAp $ BooleanInput label constraints identity

int :: String -> (Constraints Int) -> Form Int
int label constraints = liftFreeAp $ IntInput label constraints identity

number :: String -> (Constraints Number) -> Form Number
number label constraints = liftFreeAp $ NumberInput label constraints identity

char :: String -> (Constraints Char) -> Form Char
char label constraints = liftFreeAp $ CharInput label constraints identity

enumSelect :: String -> Array String -> (Constraints String) -> Form String
enumSelect label options constraints = liftFreeAp $ EnumSelect label options constraints identity

-- TODO
-- enumSelect :: forall a . BoundedEnum a => Show a => Read a => String -> Array a -> (Constraints a) -> Form a
-- enumSelect label options constraints = liftFreeAp $ EnumSelect label (show <$> options) (cmapConstraints (fromMaybe (unsafeThrow "!") <<< read) constraints) identity

-- interpreter
data WebForm a = WebForm (Widget (Dynamic (Maybe a)))

runWebForm :: forall a . WebForm a -> Widget (Dynamic (Maybe a))
runWebForm (WebForm w) = w

instance formFunctor :: Functor WebForm where
  map f (WebForm w)= WebForm (map (map (map f)) w)

instance formApplicative :: Applicative WebForm where
  pure = WebForm <<< pure <<< pure <<< pure

instance formApply :: Apply WebForm where
  apply (WebForm wdf) (WebForm wda) = WebForm $ (\dmf dma -> (<*>) <$> dmf <*> dma) <$> wdf <*> wda

bind' :: forall a b . WebForm a -> (a -> WebForm b) -> WebForm b
bind' (WebForm wdma) f = WebForm $ wdma >>= processField (runWebForm <<< f)
  where
    processField :: forall ctx out . (ctx -> Widget (Dynamic (Maybe out))) -> Dynamic (Maybe ctx) -> Widget (Dynamic (Maybe out))
    processField fieldWidget dmCtx = withDynamic dmCtx $ \mCtx -> case mCtx of
      Nothing -> pure $ pure Nothing
      Just ctx -> fieldWidget ctx
    withDynamic :: forall a b . Dynamic a -> (a -> Widget (Dynamic b)) -> Widget (Dynamic b)
    withDynamic = unsafeThrow "hole - is this possible to implement?"

webform' :: forall a . Form a -> WebForm a
webform' = foldFreeAp go
  where
    go :: forall a . FormF a -> WebForm a
    go (Section s a) = WebForm $ do
      el "h1" [] $ text s
      pure $ pure $ pure a
    go (StringInput label constraints f) = WebForm $ do
      el "div" [] do
        text label
        el "div" [] do
          dyn <- stringInputWidget constraints
          pure (map f <$> dyn)
    go (IntInput label constraints f) = WebForm $ do
      el "div" [] do
        text label
        el "div" [] do
          dyn <- intInputWidget constraints
          pure (map f <$> dyn)
    go (NumberInput label constraints f) = WebForm $ do
      el "div" [] do
        text label
        el "div" [] do
          dyn <- numberInputWidget constraints
          pure (map f <$> dyn)
    go (CharInput label constraints  f) = WebForm $ do
      el "div" [] do
        text label
        el "div" [] do
          dyn <- charInputWidget constraints
          pure (map f <$> dyn)
    go (BooleanInput _ _ (f :: Boolean -> a)) = WebForm $ do
      dyn <- pure $ pure $ pure true
      pure (map f <$> dyn)
    go (EnumSelect _ _ _ f) = WebForm $ do
      dyn <- pure $ pure $ pure "a"
      pure (map f <$> dyn)
    go (EnumMultiSelect _ _ _ f) = WebForm $ do
      dyn <- pure $ pure $ pure ["a", "b", "c"]
      pure (map f <$> dyn)

stringInputWidget :: Constraints String -> Widget (Dynamic (Maybe String))
stringInputWidget constraints = do
  ref@(Ref dyn updateRef) <- new ""
  Tuple element _ <- elDynAttr' "input" (pure mempty) (pure unit)
  Tuple velement _ <- elDynAttr' "span" (pure mempty) (pure unit)
  subscribeDyn_ (setTextInputValue element) dyn
  domChanged <- domEventWithSample (\_ -> getTextInputValue element) "input" element
  attachEvent domChanged (const >$< updateRef)
  dynText $ either head (const "") <<< valid constraints <$> dyn
  pure $ either (const Nothing) Just <<< valid constraints <$> dyn
    where
    valid :: forall a . Constraints a -> a -> Either (NonEmpty Array Error) a
    valid cs a = maybe (Right a) Left $ toNonEmpty $ mapMaybe (\c -> c a) cs
      where
      toNonEmpty :: forall a . Array a -> Maybe (NonEmpty Array a)
      toNonEmpty = map (\{ head, tail} -> NonEmpty head tail) <<< uncons

intInputWidget :: Constraints Int -> Widget (Dynamic (Maybe Int))
intInputWidget constraints = map (map (_ >>= Int.fromString)) $ stringInputWidget (cMaybeMapConstraints Int.fromString "must be integer number" constraints)

numberInputWidget :: Constraints Number -> Widget (Dynamic (Maybe Number))
numberInputWidget constraints = map (map (_ >>= Num.fromString)) $ stringInputWidget (cMaybeMapConstraints Num.fromString "must be number" constraints)

charInputWidget :: Constraints Char -> Widget (Dynamic (Maybe Char))
charInputWidget constraints = map (map (_ >>= toChar)) $ stringInputWidget (cMaybeMapConstraints toChar "must be a character" constraints)


webform :: forall a . Form a -> Widget (Dynamic (Maybe a))
webform = runWebForm <<< webform'