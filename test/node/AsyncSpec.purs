module AsyncSpec where

import Prelude hiding (append)

import BuilderSpec (newDynamic)
import Control.Monad.Cleanup (execCleanupT, runCleanupT)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Specular.FRP (current, newEvent, pull, subscribeEvent_)
import Specular.FRP.Async (RequestState(..), asyncRequestMaybe, performEvent)
import Specular.FRP.Base (readBehavior, subscribeDyn_, readDynamic)
import Specular.Internal.Effect (newRef)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (append, clear, shouldHaveValue, shouldReturn)

spec :: Spec Unit
spec = do
  describe "asyncRequestMaybe" $ do
    it "makes a request for initial value" $ do
      avar <- AVar.empty
      log <- liftEffect $ newRef []

      let request =  AVar.take avar

      _ <- execCleanupT do
        result <- asyncRequestMaybe $ pure $ Just request
        subscribeDyn_ (append log) result

      log `shouldHaveValue` [Loading]

      clear log
      AVar.put "result" avar
      log `shouldHaveValue` [Loaded "result"]

    it "makes a request when the value changes" $ do
      avar <- AVar.empty
      log <- liftEffect $ newRef []

      let request =  AVar.take avar

      Tuple dyn setDyn <- liftEffect $ newDynamic Nothing

      _ <- execCleanupT do
        result <- asyncRequestMaybe dyn
        subscribeDyn_ (append log) result

      log `shouldHaveValue` [NotRequested]

      clear log
      liftEffect $ setDyn (Just request)
      log `shouldHaveValue` [Loading]

      clear log
      AVar.put "result" avar
      log `shouldHaveValue` [Loaded "result"]

    it "ignores responses to requests older than the current" $ do
      avar1 <- AVar.empty
      avar2 <- AVar.empty
      log <- liftEffect $ newRef []

      Tuple dyn setDyn <- liftEffect $ newDynamic Nothing

      _ <- execCleanupT do
        result <- asyncRequestMaybe dyn
        subscribeDyn_ (append log) result

      log `shouldHaveValue` [NotRequested]

      clear log
      liftEffect $ setDyn $ Just $  AVar.take avar1
      liftEffect $ setDyn $ Just $  AVar.take avar2
      log `shouldHaveValue` [Loading, Loading]

      clear log
      AVar.put "result1" avar1
      log `shouldHaveValue` [] -- should be ignored, as new request is going on

      clear log
      AVar.put "result2" avar2
      log `shouldHaveValue` [Loaded "result2"]

    it "ignores out-of-order responses" $ do
      avar1 <- AVar.empty
      avar2 <- AVar.empty
      log <- liftEffect $ newRef []

      Tuple dyn setDyn <- liftEffect $ newDynamic Nothing

      _ <- execCleanupT do
        result <- asyncRequestMaybe dyn
        subscribeDyn_ (append log) result

      liftEffect $ setDyn $ Just $  AVar.take avar1
      liftEffect $ setDyn $ Just $  AVar.take avar2

      clear log
      AVar.put "result2" avar2
      log `shouldHaveValue` [Loaded "result2"]

      clear log
      AVar.put "result1" avar1
      log `shouldHaveValue` [] -- should be ignored, as this request was replaced by avar2

    it "request dynamic and status dynamic are consistent" $ do
      -- `do status <- asyncRequestMaybe request`
      -- Some relations must hold between the values of `status` and `request`:
      --
      -- - If `request == Nothing`, then `status == NotRequested`
      -- - If `request` is `Just x`, then `status` is either `Loading` or `Loaded y`,
      --   where `y` is the result of running `x`.
      --
      -- A naive implementation would expose intermediate states where these
      -- invariants don't hold. This test checks for this.

      avar <- AVar.empty

      -- In `dyn` we'll store pairs of (String, Aff String).
      -- The first string is a description, and goes to the log;
      -- the action is the request.
      Tuple dyn setDyn <- liftEffect $ newDynamic $ Tuple "Nothing" (Nothing :: Maybe (Aff String))

      let readDyn = liftEffect <<< pull <<< readBehavior <<< current

      -- In `log` we'll have pairs of (String, String)
      -- The first String is the request description, the second is the result.
      log <- liftEffect $ newRef []

      Tuple result _ <- runCleanupT do
        status <- asyncRequestMaybe $ map snd dyn
        let result = Tuple <$> map fst dyn <*> status
        subscribeDyn_ (append log) $ result
        pure result

      log `shouldHaveValue` [Tuple "Nothing" NotRequested]
      readDyn result `shouldReturn` Tuple "Nothing" NotRequested

      -- Test with immediately executed action
      clear log
      liftEffect $ setDyn $ Tuple "pure A" $ Just $ pure "A"
      log `shouldHaveValue` [Tuple "pure A" Loading, Tuple "pure A" (Loaded "A")]
      readDyn result `shouldReturn` Tuple "pure A" (Loaded "A")

      -- Test with asynchronous action
      clear log
      liftEffect $ setDyn $ Tuple "async B" $ Just $  AVar.take avar
      log `shouldHaveValue` [Tuple "async B" Loading]
      readDyn result `shouldReturn` Tuple "async B" Loading

      clear log
      AVar.put "B" avar
      log `shouldHaveValue` [Tuple "async B" (Loaded "B")]
      readDyn result `shouldReturn` Tuple "async B" (Loaded "B")

      -- Test with change to Nothing
      clear log
      liftEffect $ setDyn $ Tuple "Nothing again" Nothing
      log `shouldHaveValue` [Tuple "Nothing again" NotRequested]
      readDyn result `shouldReturn` Tuple "Nothing again" NotRequested

  it "works if initial action is synchronous" $ do
    Tuple result _ <- runCleanupT do
      status <- asyncRequestMaybe $ pure $ Just $ pure "foo"
      readDynamic status
    
    result `shouldEqual` Loaded "foo"

  describe "performEvent" $ do
    it "runs handler and pushes return value to event" $ do
      {event,fire} <- liftEffect newEvent
      log <- liftEffect $ newRef []
      _ <- liftEffect $ runCleanupT $ do
        result <- performEvent $ map
          (\x -> liftEffect (append log ("handler:" <> x)) *> pure x)
          event
        subscribeEvent_ (\x -> append log $ "result:" <> x) result

      liftEffect $ fire "A"
      liftEffect $ fire "B"

      log `shouldHaveValue` ["handler:A", "result:A", "handler:B", "result:B"]
