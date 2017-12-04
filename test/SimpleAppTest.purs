module SimpleAppTest where

import Prelude hiding (append)

import Control.Monad.Cleanup (class MonadCleanup)
import Control.Monad.IOSync.Class (class MonadIOSync)
import Data.Monoid (mempty)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Specular.Dom.Browser (Node, outerHTML)
import Specular.Dom.Builder (Builder, domEventWithSample, dynText, el, elDynAttr', text)
import Specular.Dom.Node.Class (Attrs)
import Specular.FRP (Dynamic, Event, foldDyn, leftmost)
import Specular.FRP.Fix (fixFRP)
import Specular.FRP.WeakDynamic (WeakDynamic)
import Test.Spec (Spec, describe, it)
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (ioSync, shouldReturn)
import Test.Utils.Dom (runBuilderInDiv, dispatchTrivialEvent, querySelector)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "simple counter app" $ do
  it "works" $ do
    Tuple node _ <- runBuilderInDiv $ do
      fixFRP $ view >=> control

    ioSync (outerHTML node) `shouldReturn`
      ( """<div><p>0</p>""" <>
        """<button class="increment">Increment</button>""" <>
        """<button class="decrement">Decrement</button></div>""" )

    incrementButton <- ioSync $ querySelector ".increment" node
    decrementButton <- ioSync $ querySelector ".decrement" node

    ioSync $ dispatchTrivialEvent incrementButton "click"

    ioSync (outerHTML node) `shouldReturn`
      ( """<div><p>1</p>""" <>
        """<button class="increment">Increment</button>""" <>
        """<button class="decrement">Decrement</button></div>""" )

    ioSync $ dispatchTrivialEvent decrementButton "click"
    ioSync $ dispatchTrivialEvent decrementButton "click"
    ioSync $ dispatchTrivialEvent decrementButton "click"

    ioSync (outerHTML node) `shouldReturn`
      ( """<div><p>-2</p>""" <>
        """<button class="increment">Increment</button>""" <>
        """<button class="decrement">Decrement</button></div>""" )

view ::
     { value :: WeakDynamic Int }
  -> Builder Node
    { increment :: Event Unit
    , decrement :: Event Unit
    }
view {value} = do
  el "p" $ dynText (show <$> value)

  increment <- buttonOnClick (pure $ SM.singleton "class" "increment") $ text "Increment"
  decrement <- buttonOnClick (pure $ SM.singleton "class" "decrement") $ text "Decrement"

  pure { increment, decrement }

control ::
     forall m
   . MonadIOSync m
  => MonadCleanup m
  => { increment :: Event Unit
     , decrement :: Event Unit
     }
  -> m (Tuple
    { value :: Dynamic Int }
    Unit
    )
control {increment,decrement} = do
  value <- foldDyn ($) 0 $
    leftmost
      [ (_ + 1) <$ increment
      , (_ - 1) <$ decrement
      ]
  pure (Tuple {value} unit)

buttonOnClick :: WeakDynamic Attrs -> Builder Node Unit -> Builder Node (Event Unit)
buttonOnClick attrs inner = do
  Tuple node _ <- elDynAttr' "button" attrs inner
  domEventWithSample (\_ -> pure unit) "click" node
