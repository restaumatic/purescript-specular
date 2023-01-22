module Specular.Dom.PWidget where

import Prelude

import Data.Either (Either(..), either, isLeft, isRight)
import Data.Lens (prism')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice, right)
import Data.Profunctor.Strong (class Strong, first)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (liftEffect)
import Prim.Row as Row
import Specular.Dom.Browser (Attrs, Node, TagName, (:=))
import Specular.Dom.Browser as DOM
import Specular.Dom.Builder.Class (domEventWithSample, elDynAttr')
import Specular.Dom.Builder.Class as S
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Input (getCheckboxChecked, getTextInputValue, setTextInputValue)
import Specular.FRP (Dynamic, Event, attachDynWith, changed, filterMapEvent, leftmost, never, newDynamic, newEvent, readDynamic, subscribeEvent_, uniqDyn, uniqDynBy, weaken, whenD)
import Specular.Ref (Ref, newRef, value, write)
import Type.Proxy (Proxy(..))

newtype PWidget :: Type -> Type -> Type
newtype PWidget a b = PWidget (Dynamic a -> Widget (Event b))

instance Semigroup (PWidget a b) where
  append (PWidget f1) (PWidget f2) = PWidget \dyn -> do
    b1 <- f1 dyn
    b2 <- f2 dyn
    pure $ leftmost [b1, b2]

instance Monoid (PWidget a b) where
  mempty = PWidget $ const $ pure never

derive instance Newtype (PWidget a b) _

instance Profunctor PWidget where
  dimap pre post (PWidget f) = PWidget \dyna -> do
    b <- f $ pre <$> dyna
    pure (post <$> b)

instance Strong PWidget where
  first (PWidget f) = PWidget \dynab -> do
    c <- f (fst <$> dynab)
    pure $ attachDynWith (\b c -> Tuple c b) (snd <$> dynab) c
  second (PWidget f) = PWidget \dynab -> do
    c <- f (snd <$> dynab)
    pure $ attachDynWith (\a c -> Tuple a c) (fst <$> dynab) c

instance Choice PWidget where
  left (PWidget f) = PWidget $ \dynaorb -> do
    shouldDisplay <- uniqDynBy eq (isLeft <$> dynaorb)
    {event, fire} <- newEvent
    whenD shouldDisplay do
      aorb <- readDynamic dynaorb
      case aorb of
        Left b -> do
          dyn' <- newDynamic b
          c <- f dyn'.dynamic
          subscribeEvent_ (fire <<< Left) c
          subscribeEvent_ dyn'.set $ filterMapEvent (either Just (const Nothing)) $ changed dynaorb
        _ -> pure unit
    pure event
  right (PWidget f) = PWidget $ \dynaorb -> do
    shouldDisplay <- uniqDynBy eq (isRight <$> dynaorb)
    {event, fire} <- newEvent
    whenD shouldDisplay do
      aorb <- readDynamic dynaorb
      case aorb of
        Right a -> do
          dyn' <- newDynamic a
          c <- f dyn'.dynamic
          subscribeEvent_ (fire <<< Right) c
          subscribeEvent_ dyn'.set $ filterMapEvent (either (const Nothing) Just) $ changed dynaorb
        _ -> pure unit
    pure event

-- instance Wander PWidget where
--   wander
--     :: forall s t a b
--      . (forall f. Applicative f => (a -> f b) -> s -> f t)
--     -> PWidget a b -- Dynamic a -> Widget (Event b)
--     -> PWidget s t -- Dynamic s -> Widget (Event t)
  -- wander = unsafeThrow "impossible?"

-- entry points
withRef :: forall a. Ref a -> PWidget a a -> Widget Unit
withRef ref w = do
  e <- unwrap w (value ref)
  subscribeEvent_ (write ref) e

-- PWidget primitives

-- mempty

text ∷ forall a. PWidget String a
text = withUniqDyn $ wrap \textD -> do
  S.dynText (weaken textD)
  pure never

-- PWidget combinators

-- <>


-- PWidget optics (considered as functions from PWidget to PWidget)

-- left

-- right

-- first

-- second

-- prop

-- prism

propEq
  :: forall l r1 r2 r a b
   . IsSymbol l
  => Row.Cons l a r r1
  => Row.Cons l b r r2
  => Eq a
  => Proxy l
  -> PWidget a b -> PWidget (Record r1) (Record r2)
propEq k = withUniqDyn >>> prop k

prismEq ∷ forall a s . Eq s ⇒ (s → a) → (a → Maybe s) → PWidget s s → PWidget a a
prismEq p q = withUniqDyn >>> prism' p q

withUniqDyn :: forall a s . Eq a => PWidget a s -> PWidget a s
withUniqDyn (PWidget f) = PWidget \dyn -> do
  udyn <- uniqDyn dyn
  f udyn

static :: forall a b s. a -> PWidget a s -> PWidget b s
static a (PWidget f) = PWidget \_ -> do
  f $ pure a

inside :: forall a b. TagName -> (a -> Attrs) -> (Dynamic a -> Node -> Widget (Event b)) -> PWidget a b -> PWidget a b
inside tagName attrs event wrapped = PWidget \dyn -> do
  Tuple node innerEvent <- elDynAttr' tagName (weaken dyn <#> attrs) $ unwrap wrapped dyn
  outerEvent <- event dyn node
  pure $ innerEvent <> outerEvent

-- helpers on top of primitives, combinators and optics

textInput :: (String -> Attrs) -> PWidget String String
textInput attrs = mempty # (inside "input" attrs \dyn node -> do
  liftEffect $ do
    initialValue <- readDynamic dyn
    setTextInputValue node initialValue
  subscribeEvent_ (setTextInputValue node) (changed dyn)
  domEventWithSample (\_ -> getTextInputValue node) "input" node) # withUniqDyn

checkbox :: (Boolean -> Attrs) -> PWidget Boolean Boolean
checkbox attrs = mempty # (inside "input" (\enabled -> ("type" := "checkbox") <> (if enabled then "checked" := "checked" else mempty) <> attrs enabled) \_ node -> do
  domEventWithSample (\_ -> getCheckboxChecked node) "change" node) # withUniqDyn

onClick ∷ forall a e. e -> a → Node → Widget (Event e)
onClick e _ node = do
  {event, fire} <- newEvent
  _ <- liftEffect $ DOM.addEventListener "click" fire node
  pure (e <$ event)

type Control a b =
  { controlled :: a
  , controller :: b
  }

controlled = prop (Proxy :: Proxy "controlled")
controller = prop (Proxy :: Proxy "controller")

withControl :: forall a b c. c -> PWidget (Control a c) (Control b c) -> PWidget a b
withControl c w = wrap \dyna -> do
  cref <- newRef c
  let dynac = (\controlled controller -> { controlled, controller }) <$> dyna <*> value cref
  evbc <- unwrap w dynac
  let evc = (\({ controller }) -> controller) <$> evbc
  subscribeEvent_ (write cref) evc
  pure $ (\({ controlled }) -> controlled) <$> evbc

whenControl :: forall p a b. Profunctor p => Strong p => Choice p => (b -> Boolean) -> p a a -> p (Control a b) (Control a b)
whenControl pred p = dimap (\({ controlled, controller }) -> Tuple controlled controller) (\(Tuple controlled controller) -> { controlled, controller} ) $ dimap (\(Tuple a b) -> (if pred b then Right else Left) (Tuple a b)) (either identity identity) $ right $ first p

---

-- turn "legacy" Widget into profunctor (notice: widget return value is discarded)
widget :: forall a i o. Widget a -> PWidget i o
widget w = PWidget $ const $ w *> pure never
