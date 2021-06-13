module Specular.FRPForm where

import Prelude

import Data.Maybe (Maybe (..), maybe)
import Data.String (length, null)
import Data.Traversable (for, intercalate)
import Effect (Effect)
import Specular.Dom.Element (dynText)
import Specular.Dom.Widget (runMainWidgetInBody)
import Data.Semigroup
import Data.Monoid
import Control.Monad.Maybe.Trans
import Control.Monad.Writer.Trans
import Control.Monad.Except.Trans
import Data.Tuple
import Data.Either
import Specular.Dom.Element (dynText, el, text)
import Data.Int as Int
import Data.Eq
import Data.Maybe
import Data.Maybe.Last
import Data.Newtype
import Data.Foldable
import Specular.FRP
import Specular.Ref hiding (const)
import Data.Functor.Contravariant
import Specular.Dom.Builder.Class (domEventWithSample)
import Specular.Dom.Element (dynText, el, text)
import Specular.Dom.Widget (Widget)
import Specular.Dom.Widgets.Input (getTextInputValue, setTextInputValue)
import Control.Monad.Replace
import Specular.Dom.Element
import Data.Foldable (traverse_)
import Data.Enum
import Data.Int
import Specular.Dom.Builder.Class (el', elAttr')
import Partial.Unsafe (unsafePartial)
import Specular.Dom.Node.Class ((:=))
import Specular.Dom.Browser
import Specular.Dom.Builder.Class


-- Input as monad transformer stack on top of Dynamic

type Error = String

type Input m a = ExceptT Error (MaybeT (WriterT Touch m)) a
-- ExceptT - introduces possible validation failure
-- MaybeT - introduces possible indetermination failure (e.g. we can't tell yet if valid)
-- WriterT Touch - introduces out-of-band form data, form meta-data like intact/touched property
-- also played around with ReaderT but didn't find any useful application of it

-- Input m a holds values of type: m (Tuple (Maybe (Either Error a)) Touch)
runInput :: forall a m . Monad m => Input m a -> m (Tuple (Maybe (Either Error a)) Touch)
runInput = runWriterT <<< runMaybeT <<< runExceptT

-- the intact/touched property of an input: whether it has been touched by a user or remains intact
-- handled by WriterT tranformer, must be a Monoid
data Touch = Intact | Touched | Touching

instance touchSemigroup :: Semigroup Touch where
  append Touching _ = Touching
  append _ Touching = Touching
  append Touched _ = Touched
  append _ Touched = Touched
  append _ _ = Intact

instance monoidTouch :: Monoid Touch where
  mempty = Intact

valid :: forall a m . Monad m => a -> Input m a
valid = pure

invalid :: forall a m . Monad m => Error -> Input m a
invalid = throwError

validate :: forall a m . Monad m => Either Error a -> Input m a
validate = except

validOrUndetermined :: forall a m. Monad m => Input m a -> Input m a
validOrUndetermined i = ExceptT $ MaybeT $ WriterT $ do
  value <- runInput i
  pure $ case value of
    Tuple Nothing w -> Tuple Nothing w
    Tuple (Just (Left error)) w -> Tuple Nothing w
    Tuple (Just (Right a)) w -> Tuple (Just (Right a)) w

-- or you can use shortcut functions like:
whenInputValid :: forall a m . MonadReplace m => MonadFRP m => Input Dynamic a -> (a -> m Unit) -> m Unit
whenInputValid form action = withDynamic_ (runInput form) case _ of
  Tuple (Just (Right a)) _ -> action a
  _ -> pure unit

whenInputIntact :: forall a m . MonadReplace m => MonadFRP m => Input Dynamic a -> (m Unit) -> m Unit
whenInputIntact form action = withDynamic_ (runInput form) case _ of
  Tuple _ Intact -> action
  _ -> pure unit

whenInputTouchedInvalid :: forall a m . MonadReplace m => MonadFRP m => Input Dynamic a -> (Error -> m Unit) -> m Unit
whenInputTouchedInvalid form action = withDynamic_ (runInput form) case _ of
  Tuple (Just (Left error)) Touched -> action error
  _ -> pure unit

whenInputReady :: forall a m . MonadReplace m => MonadFRP m => Input Dynamic a -> m Unit -> m Unit
whenInputReady form action = withDynamic_ (runInput form) case _ of
  Tuple (Just _) _ -> action
  _ -> pure unit


-- with functions in below one can manipulate Inputs
-- these are the things one cannot do with plain Dynamic

-- but how can we create an Input?

-- Field - a primitive, atomic Input

type Field a =
  { inputValueRef :: Ref (Tuple a Touch)
  }

field :: forall a . Monoid a => Effect (Field a)
field = do
  ref <- new (Tuple mempty mempty)
  pure {inputValueRef: ref }

writeField :: forall a . Field a -> Tuple a Touch -> Effect Unit
writeField input t = modify input.inputValueRef (\_ -> t)

fieldInput :: forall a . Field a -> Input Dynamic a
fieldInput input = ExceptT $ MaybeT $ WriterT $ do
  Tuple a w <- value input.inputValueRef
  pure (Tuple (pure (pure a)) w)

-- then we can wrap Fields with Widgets

stringFieldWidget :: forall m a . MonadDomBuilder m => MonadFRP m => Field String -> m (Tuple Node a) -> m Unit
stringFieldWidget field m = do
  Tuple element _ <- m -- elAttr' "input" mempty (pure unit)
  _ <- domEventWithSample (\_ -> do
    str <- getTextInputValue element
    writeField field (Tuple str Touched)
    ) "input" element
  subscribeEvent_ (setTextInputValue element) (changed (fieldDyn field))
    where
    fieldDyn :: forall a . Field a -> Dynamic a
    fieldDyn field = (\(Tuple a b) -> a) <$> value field.inputValueRef

-- Form is a Widget that provides input and "callback" to modify input fields

type Form m i o = Widget (Tuple (Input m i) (o -> Effect Unit))
