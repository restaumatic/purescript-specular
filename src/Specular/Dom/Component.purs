module Specular.Dom.Component
  where

import Prelude

import Control.Monad.Replace (destroySlot, newSlot, replaceSlot)
import Data.Array (cons, drop, fold, fromFoldable, take, toUnfoldable, (!!))
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.Lens (left, prism', second)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, modify, unwrap, wrap)
import Data.Profunctor (class Profunctor, dimap, lcmap, rmap)
import Data.Profunctor.Choice (class Choice, right)
import Data.Profunctor.Strong (class Strong, first)
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (class Unfoldable)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (new, read)
import Prim.Row as Row
import Specular.Dom.Browser (Attrs, Node, TagName, setAttributes, (:=))
import Specular.Dom.Browser as DOM
import Specular.Dom.Builder (Builder)
import Specular.Dom.Builder.Class (elDynAttr')
import Specular.Dom.Builder.Class as S
import Specular.Dom.Widgets.Input (setCheckboxChecked, setTextInputValue)
import Specular.FRP (weaken)
import Specular.Ref (value, write)
import Specular.Ref as Ref
import Type.Proxy (Proxy(..))

type Path = Array Hop

data Hop = HopLeft | HopRight | HopFirst | HopSecond

derive instance Generic Hop _
derive instance Eq Hop

instance Show Hop where
  show = genericShow

-- Dynamic for lenses
type WithPath a =  
  { path :: Path
  , value :: a
  }

newtype Component :: Type -> Type -> Type
-- newtype Component a b = Component (Dynamic (WithPath a) -> Widget (Event (WithPath b)))
newtype Component i o = Component (Builder Unit (i -> (o -> Effect Unit) -> Effect Unit))

derive instance Newtype (Component a b) _

instance Semigroup (Component a b) where
  append c1 c2 = wrap do
    f1 <- unwrap c1
    f2 <- unwrap c2
    pure \i ocallback -> do
      f1 i ocallback
      f2 i ocallback

instance Monoid (Component a b) where
  mempty = Component mempty

replace :: forall a b. Component a b -> Component a b -> Component a b
replace = const

instance Profunctor Component where
  dimap pre post c = wrap do
      f <- unwrap c
      pure \i ocallback -> do
        f (pre i) \o -> ocallback (post o)

instance Strong Component where
  first component = wrap do
    f <- unwrap component
    pure $ \ab abcallback -> do
      f (fst ab) \a -> abcallback $ Tuple a (snd ab)
  second component = wrap do
    f <- unwrap component
    pure $ \ab abcallback -> do
      f (snd ab) \b -> abcallback $ Tuple (fst ab) b

-- noStrongComponent :: forall a. Component a Unit
-- noStrongComponent = wrap $ \dyn -> pure $ unit <$ changed dyn

-- noStrongComponent' :: forall a b. b -> Component a b
-- noStrongComponent' b = rmap (const b) noStrongComponent

instance Choice Component where
  left component = wrap do
    slot <- newSlot
    pure $ \ab abcallback -> do
      case ab of
        Left a -> do
          f <- replaceSlot slot $ unwrap component
          f a \a -> abcallback (Left a)
        Right _ -> do
          destroySlot slot
  right component = wrap do
    slot <- newSlot
    pure $ \ab abcallback -> do
      case ab of
        Right b -> do
          f <- replaceSlot slot $ unwrap component
          f b \b -> abcallback (Right b)
        Left _ -> do
          destroySlot slot

noChoiceComponent :: forall a. Component a Void
noChoiceComponent = wrap $ pure mempty

noChoiceComponent' :: forall a b. Component a b
noChoiceComponent' = rmap absurd noChoiceComponent

-- instance Wander Component where
--   wander
--     :: forall s t a b
--      . (forall f. Applicative f => (a -> f b) -> s -> f t)
--     -> Component a b -- Dynamic a -> Widget (Event b)
--     -> Component s t -- Dynamic s -> Widget (Event t)
  -- wander = unsafeThrow "impossible?"

-- entry points

-- lala :: forall a b. Dynamic a -> (b -> Effect Unit) -> ComponentWrapper Identity a b -> Widget Unit
-- lala dyn callback w' = do
--   let (Identity w) = unwrap w'
--   b <- unwrap w (dyn <#> \value -> { path: [], value })
--   subscribeEvent_ callback (b <#> _.value)

renderComponent :: forall a. a -> ComponentWrapper Identity a a -> Builder Unit Unit
renderComponent a componentw = do  
  let (Identity component) = unwrap componentw
  f <- unwrap component
  let callback a = f a callback
  liftEffect $ callback a

-- Component primitives

-- mempty

text :: forall f a. Applicative f => ComponentWrapper f String a
text = withUniqDyn $ wrap $ pure $ wrap do
  slot <- newSlot
  pure \t _ -> do
    replaceSlot slot $ S.text t

-- Component combinators

-- <>

-- >>>>


-- Component optics (considered as functions from Component to Component)

-- left

-- right

-- first

-- second

-- prop

-- prism

propEq
  :: forall f l r1 r2 r a b
   . IsSymbol l
  => Row.Cons l a r r1
  => Row.Cons l b r r2
  => Eq a
  => Functor f
  => Proxy l
  -> ComponentWrapper f a b -> ComponentWrapper f (Record r1) (Record r2)
propEq k = withUniqDyn >>> prop k

prismEq ∷ forall f a s . Functor f => Eq s ⇒ (s → a) → (a → Maybe s) → ComponentWrapper f s s → ComponentWrapper f a a
prismEq p q = withUniqDyn >>> prism' p q

withUniqDyn :: forall f i o. Functor f => Eq i => ComponentWrapper f i o -> ComponentWrapper f i o
withUniqDyn = modify $ map \component -> wrap do
  f <- unwrap component
  lastRef <- liftEffect $ new Nothing
  pure \i ocallback -> do
    last <- read lastRef
    unless (Just i == last) $ f i ocallback


static :: forall f a b s. Functor f => a -> ComponentWrapper f a s -> ComponentWrapper f b s
static a = unwrap >>> map (\component -> wrap do
  f <- unwrap component
  pure \_ _ -> f a mempty) >>> wrap

inside :: forall f a b. Functor f => TagName -> (a -> Attrs) -> (a -> Node -> (b -> Effect Unit) -> Effect Unit) -> ComponentWrapper f a b -> ComponentWrapper f a b
inside tagName attrs event = modify $ map \component -> wrap do
  Tuple node f <- elDynAttr' tagName (weaken (pure mempty)) $ unwrap component -- TODO: stop using (Weak)Dynamic
  pure \a bcallback -> do
    f a bcallback
    setAttributes node (attrs a)
    -- TODO: propagate events from wrapper?
    -- event a node bcallback
  -- outerEvent <- event dyn node
  -- pure $ innerEvent <> outerEvent

-- helpers on top of primitives, combinators and optics

textInput :: forall f. Applicative f => (String -> Attrs) -> ComponentWrapper f String String
textInput attrs = mempty # inside "input" attrs \str node callback -> do
  setTextInputValue node str
  -- (domEventWithSample (\_ -> getTextInputValue node <#> \value -> {path: [], value}) "input" node)

checkbox :: forall f. Applicative f => (Boolean -> Attrs) -> ComponentWrapper f Boolean Boolean
checkbox attrs = mempty # inside "input" (\enabled -> ("type" := "checkbox") <> (if enabled then "checked" := "checked" else mempty) <> attrs enabled) \bool node callback -> do
  setCheckboxChecked node bool
  -- domEventWithSample (\_ -> getCheckboxChecked node <#> \value -> { path: [], value }) "change" node

radio :: forall f. Applicative f => (Boolean -> Attrs) -> ComponentWrapper f Boolean Boolean
radio attrs = mempty # inside "input" (\enabled -> ("type" := "radio") <> (if enabled then "checked" := "checked" else mempty) <> attrs enabled) \value node callback -> do
  setCheckboxChecked node value
  -- onDomEvent "change" node (\_ -> getCheckboxChecked node >>= callback)


-- radio :: forall a b f. Applicative f => (a -> Attrs) -> ComponentWrapper f a b
-- radio attrs = mempty # (inside "input" (\enabled -> ("type" := "checkbox") <> (if enabled then "checked" := "checked" else mempty) <> attrs enabled) \_ node -> do
--   domEventWithSample (\_ -> getCheckboxChecked node <#> \value -> { path: [], value }) "change" node)


onClick ∷ forall a. a → Node → (a -> Effect Unit) -> Effect Unit
onClick a node callback = void $ DOM.addEventListener "click" (\_ -> callback a) node

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

-- withControl :: forall f a b c. Functor f => c -> ComponentWrapper f (Control a c) (Control b c) -> ComponentWrapper f a b
-- withControl c = unwrap >>> (map \component -> Component \dyna -> do
--   cref <- newRef c
--   let dynac = (\controlled controller -> { controlled, controller }) <$> dyna <*> value cref
--   evbc <- unwrap component dynac
--   let evc = (\({ controller }) -> controller) <$> evbc
--   subscribeEvent_ (write cref) evc
--   pure $ (\({ controlled }) -> controlled) <$> evbc) >>> wrap

whenControl :: forall p a b. Profunctor p => Strong p => Choice p => (b -> Boolean) -> p a a -> p (Control a b) (Control a b)
whenControl pred p = dimap (\({ controlled, controller }) -> Tuple controlled controller) (\(Tuple controlled controller) -> { controlled, controller} ) $ dimap (\(Tuple a b) -> (if pred b then Right else Left) (Tuple a b)) (either identity identity) $ right $ first p

--- BELOW ARE JUST SCATCHES

-- mergeUnit :: Component Unit Void
-- mergeUnit = mempty

-- merge :: forall a b c d . Component a b -> Component c d -> Component (Tuple a c) (Either b d)
-- merge (Component w1) (Component w2) = Component \ac -> do
--   b <- w1 (fst <$> ac)
--   d <- w2 (snd <$> ac)
--   pure ((Left <$> b) <> (Right <$> d))

adaptInput :: forall a b c. (c -> a) -> Component a b -> Component c b
adaptInput f = lcmap f

adaptOutput :: forall a b c. (b -> c) -> Component a b -> Component a c
adaptOutput f = rmap f

-- enrich :: forall a b . Component a b -> Component a (Tuple a b)
-- enrich (Component w) = Component \dyna -> do
--   eventb <- w dyna
--   pure (attachDynWith Tuple dyna eventb)

-- turn "legacy" Widget into profunctor (notice: widget return value is discarded)
-- widget :: forall a i o. Widget a -> Component i o
-- widget w = Component $ const $ w *> pure never

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

--

-- composeOptics1 :: forall p. Profunctor p => (p a b -> p s t) -> (p a' b' -> p s' t') -> (p (Tuple a a') (Tuple b b') -> p (Tuple s s') (Tuple t t'))

-- composeOptics2 :: forall p. Profunctor p => (p a b -> p s t) -> (p a' b' -> p s t) -> (p (Tuple a a') (Tuple b b') -> p s t)

-- composeOptics3 :: forall p. Profunctor p => (p a a -> p s s) -> (p b b -> p s s) -> (p (Tuple a b) (Tuple a b) -> p s s)


-- composeOptics :: forall p. Profunctor p, Semigroup p => (p a b -> p s t) -> (p a' b' -> p s' t') -> (p (Tuple a a') (Tuple b b') -> p (Tuple s s') (Tuple t t'))

-- composeOptics :: forall p a b c d. Profunctor p => Semigroup (p a b) => p a b -> p c d -> p (Tuple a c) (Tuple b d)
-- composeOptics p1 p2 = first p1 <> second p2


-- composeOptics o o' p = let
  -- first 


-- Foldables

nth :: forall a f p. Foldable f => Unfoldable f => Profunctor p => Strong p => Choice p => Int -> p a a -> p (f a) (f a)
nth n p = dimap (fromFoldable) toUnfoldable $ dimap (\array -> maybe (Left array) (\element -> Right $ Tuple (Tuple (take n array) (drop (n+1) array)) element) (array !! n)) (case _ of
  Left array -> array
  Right (Tuple (Tuple preceding following) element) -> preceding <> element `cons` following) (right (second p)) 


-- 

swallow :: forall f a b c. Functor f => ComponentWrapper f a b -> ComponentWrapper f a c
swallow = unwrap >>> map (\component -> wrap do
  f <- unwrap component
  pure $ \a _ -> f a mempty) >>> wrap

swallow' :: forall a. Component a Void
swallow' = wrap $ mempty

hide :: forall a b s t. Component a b -> Component s t
hide = const mempty

--

newtype Cayley :: forall k1 k2 k3. (k1 -> Type) -> (k2 -> k3 -> k1) -> k2 -> k3 -> Type
newtype Cayley f p a b = Cayley (f (p a b))

derive instance Newtype (Cayley f p a b) _

instance (Functor f, Profunctor p) => Profunctor (Cayley f p) where
  dimap f g = wrap <<< map (dimap f g) <<< unwrap

instance (Apply f, Profunctor p, Semigroup (p a b)) => Semigroup (Cayley f p a b) where
  append c1 c2 = wrap $ append <$> unwrap c1 <*> unwrap c2

instance (Functor f, Strong p) => Strong (Cayley f p) where
  first  = wrap <<< map first <<< unwrap
  second = wrap <<< map second <<< unwrap

instance (Functor f, Choice p) => Choice (Cayley f p) where
  left   = wrap <<< map left <<< unwrap
  right  = wrap <<< map right <<< unwrap

instance (Applicative f, Profunctor p, Monoid (p a b)) => Monoid (Cayley f p a b) where
  mempty = wrap $ pure mempty

type ComponentWrapper f a b = Cayley f Component a b

-- cast :: forall s t a. (s -> a) -> Lens s t a Void
-- cast f = lens f (const absurd)

cast :: forall p s a t. Profunctor p => (s -> a) -> p a Void -> p s t 
cast f = dimap f absurd

mockCast :: forall p s a t. Profunctor p => String -> a -> p a Void -> p s t 
mockCast functionName functionValue = cast (const functionValue)

-- when :: forall a p. Profunctor p => Choice p => String -> a -> p a b -> p s t
-- when  


-- when :: forall a p. Profunctor p => Choice p => String -> a -> p a b -> p s t 
-- when functionName functionValue = 

-- whenCast :: forall p s. Profunctor p => Strong p => Choice p => p s Boolean -> p s s -> p s s
-- -- whenCast cast p = p # rmap (\s -> Tuple s s) # first cast # rmap (\s checked -> if checked then Right s else Left unit) # right identity
-- whenCast cast p =   

-- invariantFailedMessage :: Cast s String 
-- invariantFailedMessage :: forall p. Profunctor p => Strong p => Choice p=> VirtualField p s Boolean = prism

--

-- variadic function to append ComponentWrappers
component' ::  forall f a b cws. ComponentWrappers f a b cws => (ComponentWrapper f a b -> ComponentWrapper f a b) -> cws
component' optics = cappend optics []

component = component' identity
inside' t as es = component' (inside t as es)


class ComponentWrappers f a b cws | cws -> f a b where
  cappend :: (ComponentWrapper f a b -> ComponentWrapper f a b) -> Array (ComponentWrapper f a b) -> cws
 
instance Applicative f => ComponentWrappers f a b (ComponentWrapper f a b) where
  cappend optics cws = fold cws
  
instance ComponentWrappers f a b r => ComponentWrappers f a b (ComponentWrapper f a b -> r) where
  cappend optics cws cw = cappend identity (cws <> [cw])


-- inside' ::  forall p a b cws. ComponentWrappers p a b cws => TagName -> (a -> Attrs) -> (Dynamic (WithPath a) -> Node -> Widget (Event (WithPath b))) -> cws
-- inside' tag attrs events cws = inside tag attrs events $ component cws
-- inside' ∷ ∀ (b1714 ∷ Type) (f1717 ∷ Type -> Type) (a1718 ∷ Type) (b1719 ∷ Type) (t1720 ∷ Type -> Type) (t1721 ∷ Type) (t1722 ∷ Type). Functor f1717 ⇒ ComponentWrappers t1720 t1721 t1722 (b1714 -> Cayley f1717 Component a1718 b1719) ⇒ String → (a1718 → Object String) → (Dynamic { path ∷ Array Hop , value ∷ a1718 } → Node → Builder Unit (Event { path ∷ Array Hop , value ∷ b1719 } ) ) → b1714 → Cayley f1717 Component a1718 b1719
-- inside' :: forall a b. TagName -> (a -> Attrs) -> (Dynamic (WithPath a) -> Node -> Widget (Event (WithPath b))) -> Unit
-- inside' tag attrs events = component (inside tag attrs events)

-- (ComponentWrapper f a b -> ComponentWrapper f a b) instantiates ComponentWrappers

-- instance ComponentWrappers f a b (ComponentWrapper f a b -> ComponentWrapper f a b) 

-- foo :: forall f a b. Applicative f => Array (ComponentWrapper f a b) -> (ComponentWrapper f a b -> ComponentWrapper f a b)
-- foo cws = cappend cws


