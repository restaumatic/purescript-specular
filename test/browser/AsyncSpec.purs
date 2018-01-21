-- NB: this module is in browser tests, not node tests, because `asyncRequest`
-- requires MonadWidget. This should change in the future.
module AsyncSpec where

import Prelude hiding (append)

import BuilderSpec (newDynamic)
import Control.Monad.Aff.AVar (makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Cleanup (runCleanupT)
import Control.Monad.IOSync.Class (liftIOSync)
import Data.IORef (newIORef)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Specular.FRP (newEvent, subscribeEvent_)
import Specular.FRP.Async (RequestState(..), asyncRequestMaybe, performEvent)
import Specular.FRP.Base (subscribeDyn_)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (append, clear, ioSync, shouldHaveValue)
import Test.Utils.Dom (runBuilderInDiv)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = do
  describe "asyncRequestMaybe" $ do
    it "makes a request for initial value" $ do
      avar <- makeEmptyVar
      log <- ioSync $ newIORef []

      let request = liftAff $ takeVar avar

      _ <- runBuilderInDiv $ do
        result <- asyncRequestMaybe $ pure $ Just request
        subscribeDyn_ (append log) result

      log `shouldHaveValue` [Loading]

      clear log
      putVar "result" avar
      log `shouldHaveValue` [Loaded "result"]

    it "makes a request when the value changes" $ do
      avar <- makeEmptyVar
      log <- ioSync $ newIORef []

      let request = liftAff $ takeVar avar

      Tuple dyn setDyn <- ioSync $ newDynamic Nothing

      _ <- runBuilderInDiv $ do
        result <- asyncRequestMaybe dyn
        subscribeDyn_ (append log) result

      log `shouldHaveValue` [NotRequested]

      clear log
      ioSync $ setDyn (Just request)
      log `shouldHaveValue` [Loading]

      clear log
      putVar "result" avar
      log `shouldHaveValue` [Loaded "result"]

    it "ignores responses to requests older than the current" $ do
      avar1 <- makeEmptyVar
      avar2 <- makeEmptyVar
      log <- ioSync $ newIORef []

      Tuple dyn setDyn <- ioSync $ newDynamic Nothing

      _ <- runBuilderInDiv $ do
        result <- asyncRequestMaybe dyn
        subscribeDyn_ (append log) result

      log `shouldHaveValue` [NotRequested]

      clear log
      ioSync $ setDyn $ Just $ liftAff $ takeVar avar1
      ioSync $ setDyn $ Just $ liftAff $ takeVar avar2
      log `shouldHaveValue` [Loading, Loading]

      clear log
      putVar "result1" avar1
      log `shouldHaveValue` [] -- should be ignored, as new request is going on

      clear log
      putVar "result2" avar2
      log `shouldHaveValue` [Loaded "result2"]

    it "ignores out-of-order responses" $ do
      avar1 <- makeEmptyVar
      avar2 <- makeEmptyVar
      log <- ioSync $ newIORef []

      Tuple dyn setDyn <- ioSync $ newDynamic Nothing

      _ <- runBuilderInDiv $ do
        result <- asyncRequestMaybe dyn
        subscribeDyn_ (append log) result

      ioSync $ setDyn $ Just $ liftAff $ takeVar avar1
      ioSync $ setDyn $ Just $ liftAff $ takeVar avar2

      clear log
      putVar "result2" avar2
      log `shouldHaveValue` [Loaded "result2"]

      clear log
      putVar "result1" avar1
      log `shouldHaveValue` [] -- should be ignored, as this request was replaced by avar2

  describe "performEvent" $ do
    it "runs handler and pushes return value to event" $ do
      {event,fire} <- ioSync newEvent
      log <- ioSync $ newIORef []
      _ <- ioSync $ runCleanupT $ do
        result <- performEvent $ map
          (\x -> liftIOSync (append log ("handler:" <> x)) *> pure x)
          event
        subscribeEvent_ (\x -> append log $ "result:" <> x) result

      ioSync $ fire "A"
      ioSync $ fire "B"

      log `shouldHaveValue` ["handler:A", "result:A", "handler:B", "result:B"]
