module Specular.Dom.Component where

import Prelude

import Data.Either (Either(..), either, isLeft, isRight)
import Data.Lens (prism')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, dimap, lcmap, rmap)
import Data.Profunctor.Choice (class Choice, right)
import Data.Profunctor.Strong (class Strong, first)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Prim.Row as Row
import Specular.Dom.Browser (Attrs, Node, TagName, (:=))
import Specular.Dom.Browser as DOM
import Specular.Dom.Builder.Class (domEventWithSample, elDynAttr')
import Specular.Dom.Builder.Class as S
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Input (getCheckboxChecked, getTextInputValue, setTextInputValue)
import Specular.FRP (Dynamic, Event, attachDynWith, changed, filterMapEvent, leftmost, never, newDynamic, newEvent, readDynamic, subscribeEvent_, tagDyn, uniqDyn, uniqDynBy, weaken, whenD)
import Specular.Ref (newRef, value, write)
import Specular.Ref as Ref
import Type.Proxy (Proxy(..))

newtype Component :: Type -> Type -> Type
newtype Component a b = Component (Dynamic a -> Widget (Event b))

instance Semigroup (Component a b) where
  append (Component f1) (Component f2) = Component \dyn -> do
    b1 <- f1 dyn
    b2 <- f2 dyn
    pure $ leftmost [b1, b2]

instance Monoid (Component a b) where
  mempty = Component $ const $ pure never

derive instance Newtype (Component a b) _

instance Profunctor Component where
  dimap pre post (Component f) = Component \dyna -> do
    b <- f $ pre <$> dyna
    pure (post <$> b)

instance Strong Component where
  first (Component f) = Component \dynab -> do
    c <- f (fst <$> dynab)
    pure $ attachDynWith (\b c -> Tuple c b) (snd <$> dynab) c
  second (Component f) = Component \dynab -> do
    c <- f (snd <$> dynab)
    pure $ attachDynWith (\a c -> Tuple a c) (fst <$> dynab) c

instance Choice Component where
  left (Component f) = Component $ \dynaorb -> do
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
  right (Component f) = Component $ \dynaorb -> do
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

-- instance Wander Component where
--   wander
--     :: forall s t a b
--      . (forall f. Applicative f => (a -> f b) -> s -> f t)
--     -> Component a b -- Dynamic a -> Widget (Event b)
--     -> Component s t -- Dynamic s -> Widget (Event t)
  -- wander = unsafeThrow "impossible?"

-- entry points

lala :: forall a b. Dynamic a -> (b -> Effect Unit) -> Component a b -> Widget Unit
lala dyn callback w = do
  b <- unwrap w dyn
  subscribeEvent_ callback b

renderComponent :: forall a. a -> Component a a -> Widget Unit
renderComponent a w = do
  ref <-liftEffect $ newRef a
  lala (value ref) (write ref) w

-- Component primitives

-- mempty

text ∷ forall a. Component String a
text = withUniqDyn $ wrap \textD -> do
  S.dynText (weaken textD)
  pure never

-- Component combinators

-- <>


-- Component optics (considered as functions from Component to Component)

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
  -> Component a b -> Component (Record r1) (Record r2)
propEq k = withUniqDyn >>> prop k

prismEq ∷ forall a s . Eq s ⇒ (s → a) → (a → Maybe s) → Component s s → Component a a
prismEq p q = withUniqDyn >>> prism' p q

withUniqDyn :: forall a s . Eq a => Component a s -> Component a s
withUniqDyn (Component f) = Component \dyn -> do
  udyn <- uniqDyn dyn
  f udyn

static :: forall a b s. a -> Component a s -> Component b s
static a (Component f) = Component \_ -> do
  f $ pure a

inside :: forall a b. TagName -> (a -> Attrs) -> (Dynamic a -> Node -> Widget (Event b)) -> Component a b -> Component a b
inside tagName attrs event wrapped = Component \dyn -> do
  Tuple node innerEvent <- elDynAttr' tagName (weaken dyn <#> attrs) $ unwrap wrapped dyn
  outerEvent <- event dyn node
  pure $ innerEvent <> outerEvent

-- helpers on top of primitives, combinators and optics

textInput :: (String -> Attrs) -> Component String String
textInput attrs = mempty # (inside "input" attrs \dyn node -> do
  liftEffect $ do
    initialValue <- readDynamic dyn
    setTextInputValue node initialValue
  subscribeEvent_ (setTextInputValue node) (changed dyn)
  domEventWithSample (\_ -> getTextInputValue node) "input" node) # withUniqDyn

checkbox :: (Boolean -> Attrs) -> Component Boolean Boolean
checkbox attrs = mempty # (inside "input" (\enabled -> ("type" := "checkbox") <> (if enabled then "checked" := "checked" else mempty) <> attrs enabled) \_ node -> do
  domEventWithSample (\_ -> getCheckboxChecked node) "change" node) # withUniqDyn

onClick ∷ forall a. Dynamic a → Node → Widget (Event a)
onClick dyna node = do
  {event, fire} <- newEvent
  _ <- liftEffect $ DOM.addEventListener "click" fire node
  pure $ tagDyn dyna (unit <$ event)

type Control a b =
  { controlled :: a
  , controller :: b
  }

controlled :: forall r811 a812 b813 p.
  Strong p => p a812 b813
              -> p
                   { controlled :: a812
                   | r811
                   }
                   { controlled :: b813
                   | r811
                   }
controlled = prop (Proxy :: Proxy "controlled")
controller :: forall r801 a802 b803 p.
  Strong p => p a802 b803
              -> p
                   { controller :: a802
                   | r801
                   }
                   { controller :: b803
                   | r801
                   }
controller = prop (Proxy :: Proxy "controller")

withControl :: forall a b c. c -> Component (Control a c) (Control b c) -> Component a b
withControl c w = wrap \dyna -> do
  cref <- newRef c
  let dynac = (\controlled controller -> { controlled, controller }) <$> dyna <*> value cref
  evbc <- unwrap w dynac
  let evc = (\({ controller }) -> controller) <$> evbc
  subscribeEvent_ (write cref) evc
  pure $ (\({ controlled }) -> controlled) <$> evbc

whenControl :: forall p a b. Profunctor p => Strong p => Choice p => (b -> Boolean) -> p a a -> p (Control a b) (Control a b)
whenControl pred p = dimap (\({ controlled, controller }) -> Tuple controlled controller) (\(Tuple controlled controller) -> { controlled, controller} ) $ dimap (\(Tuple a b) -> (if pred b then Right else Left) (Tuple a b)) (either identity identity) $ right $ first p

--- BELOW ARE JUST SCATCHES

mergeUnit :: forall a . Component Unit a
mergeUnit = mempty

merge :: forall a b c d . Component a b -> Component c d -> Component (Tuple a c) (Either b d)
merge (Component w1) (Component w2) = Component \ac -> do
  b <- w1 (fst <$> ac)
  d <- w2 (snd <$> ac)
  pure ((Left <$> b) <> (Right <$> d))

adaptInput :: forall a b c. (c -> a) -> Component a b -> Component c b
adaptInput f = lcmap f

adaptOutput :: forall a b c. (b -> c) -> Component a b -> Component a c
adaptOutput f = rmap f

enrich :: forall a b . Component a b -> Component a (Tuple a b)
enrich (Component w) = Component \dyna -> do
  eventb <- w dyna
  pure (attachDynWith Tuple dyna eventb)

foo :: forall a b c. (b -> Aff Unit) -> Component a b -> Component a c
foo f (Component w) = Component \dyna -> do
  eventb <- w dyna
  subscribeEvent_ (\e -> launchAff_ $ f e) eventb
  pure never

bar :: forall a b c. (a -> Aff a) -> Component a a -> Component a a
bar f (Component w) = Component \dyna -> do
  eventb <- w dyna
  {event, fire} <- newEvent
  subscribeEvent_ (\e -> launchAff_ (f e >>= \e' -> liftEffect $ fire e')) eventb
  pure $ eventb <> event


-- turn "legacy" Widget into profunctor (notice: widget return value is discarded)
widget :: forall a i o. Widget a -> Component i o
widget w = Component $ const $ w *> pure never

data Ref'' e d = Ref'' (Event e -> Effect (Dynamic d))

-- newRef'' :: forall a. a -> Ref'' a a
-- newRef'' a = Ref'' $ \e -> do
--   ref <- Ref.newRef a
--   subscribeEvent (write ref) e
--   pure $ value ref

instance Profunctor Ref'' where
 dimap pre post (Ref'' f) = Ref'' (\e' -> map post <$> f (pre <$> e'))

-- instance Strong Ref'' where
--   first (Ref'' f) = Ref'' $ \ab -> do
--     d <- f (fst <$> ab)


-- instance Choice

data Ref' e d = Ref' (Dynamic d) (e -> Effect Unit)

newRef' :: forall a. a -> Effect (Ref' a a)
newRef' a = do
  ref <- Ref.newRef a
  pure $ Ref' (value ref) (write ref)

instance Profunctor Ref' where
  dimap pre post (Ref' dyn action) = Ref' (post <$> dyn) (pre >>> action)

-- instance Costrong Ref' where
--   -- unfirst (Ref dyn action) = Ref (fst <$> dyn) (fst >>> action)
--   unsecond (Ref dyn action) = Ref (fst <$> dyn) (fst >>> action)

instance Choice Ref' where
  left (Ref' dyn action) = Ref' (Left <$> dyn) (case _ of
    Left e -> action e
    _ -> pure unit)
  right (Ref' dyn action) = Ref' (Right <$> dyn) (case _ of
    Right e -> action e
    _ -> pure unit)

data Option
  = OptionInt Int
  | OptionString String
  | OptionBoolean Boolean

optionInt :: forall p. Choice p => p Int Int -> p Option Option
optionInt = prism' OptionInt (case _ of
  OptionInt o -> Just o
  _ -> Nothing)
optionString :: forall p. Choice p => p String String -> p Option Option
optionString = prism' OptionString (case _ of
  OptionString o -> Just o
  _ -> Nothing)

foo'' :: Effect Unit
foo'' = do
  ref <- newRef' 2
  let (Ref' dyn action) = ref # optionInt
  action (OptionInt 3)
  action (OptionString "2")
  pure unit
