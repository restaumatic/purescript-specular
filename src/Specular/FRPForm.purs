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


-- Input as monad transformer stack on top of Dynamic

type Error = String

type Input a = ExceptT Error (WriterT Touch Dynamic) a

-- ExceptT - introduces possible validation failure
-- WriterT Touch - introduces out-of-band form data, form meta-data like intact/touched property
-- also played around with ReaderT but didn't find any useful application of it

-- the intact/touched propery of the form: whether it has been touched by a user input or remains intact
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

-- unwraping Dynamic of a Input, then you can handle Dynamic as usual
inputDynamic :: forall a . Input a -> Dynamic (Tuple (Either Error a) Touch)
inputDynamic form = runWriterT (runExceptT form)

-- or you can use shortcut functions like:
whenInputCorrect :: forall a m . MonadReplace m => MonadFRP m => Input a -> (a -> m Unit) -> m Unit
whenInputCorrect form action = withDynamic_ (inputDynamic form) case _ of
  Tuple (Right a) _ -> action a
  _ -> pure unit

whenInputIntact :: forall a m . MonadReplace m => MonadFRP m => Input a -> (m Unit) -> m Unit
whenInputIntact form action = withDynamic_ (inputDynamic form) case _ of
  Tuple _ Intact -> action
  _ -> pure unit

whenInputTouchedIncorrect :: forall a m . MonadReplace m => MonadFRP m => Input a -> (Error -> m Unit) -> m Unit
whenInputTouchedIncorrect form action = withDynamic_ (inputDynamic form) case _ of
  Tuple (Left error) Touched -> action error
  _ -> pure unit

-- with functions in below one can manipulate Inputs
-- these are the things one cannot do with plain Dynamic
eitherOf :: forall a e . Input (Either Error a) -> Input a
eitherOf i = ExceptT $ WriterT $ do
  mmaw <-  runWriterT (runExceptT i)
  pure $ case mmaw of
    Tuple (Right (Right a)) w -> Tuple (Right a) w
    Tuple (Right (Left error)) w -> Tuple (Left error) w
    Tuple (Left error) w -> Tuple (Left "!") w


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

fieldInput :: forall a . Field a -> Input a
fieldInput input = ExceptT $ WriterT $ do
  Tuple a w <- value input.inputValueRef
  pure (Tuple (pure a) w)

-- then we can wrap Fields with Widgets

stringFieldWidget :: Field String -> Widget Unit
stringFieldWidget field = do
  Tuple element _ <- elAttr' "input" mempty (pure unit)
  domChanged <- domEventWithSample (\_ -> do
    str <- getTextInputValue element
    writeField field (Tuple str Touched)
    ) "input" element
  -- on domChanged (\str -> writeField field (Tuple str Touched))
  subscribeEvent_ (setTextInputValue element) (changed (fieldDyn field))
    where
    fieldDyn :: forall a . Field a -> Dynamic a
    fieldDyn field = (\(Tuple a b) -> a) <$> value field.inputValueRef

-- Form is a Widget that provides input and "callback" to modify input fields

type Form i o = Widget (Tuple (Input i) (o -> Effect Unit))
