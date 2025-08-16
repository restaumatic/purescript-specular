module MapAsyncSpec where

import Prelude hiding (append)

import Control.Monad.Cleanup (execCleanupT, runCleanupT)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Ref as ERef
import Specular.FRP.Base (AsyncComputation(..), mapAsync, readDynamic, subscribeDyn_)
import Specular.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Utils (append, clear, shouldHaveValue, shouldReturn, yieldAff)

spec :: Spec Unit
spec = describe "mapAsync" do
    it "makes a request for initial value" do
      avar <- AVar.empty
      log <- liftEffect $ ERef.new []

      let request = AVar.take avar
      let d = mapAsync Async (pure request)

      _ <- execCleanupT do
        subscribeDyn_ (append log <<< show) d

      log `shouldHaveValue` [ "(InProgress Nothing)" ]

      clear log
      AVar.put 1 avar
      yieldAff

      log `shouldHaveValue` [ "(Finished (Right 1))" ]

    it "doesn't make a request when not subscribed" do
      log <- liftEffect $ ERef.new []

      let request = liftEffect $ append log "requested"
      let d = mapAsync Async (pure request)

      yieldAff

      log `shouldHaveValue` []

      -- Do something with `d`, to prevent it being DCEd (I think this works, but not sure...)
      _ <- execCleanupT do
        subscribeDyn_ (append log <<< show) d
      pure unit

    it "forces sync Affs to run after reporting Loading" do
      log <- liftEffect $ ERef.new []

      let request = liftEffect $ append log "requested" *> pure 1
      let d = mapAsync Async (pure request)

      _ <- execCleanupT do
        subscribeDyn_ (append log <<< ("sub1: " <> _) <<< show) d

      yieldAff

      log `shouldHaveValue`
        ["sub1: (InProgress Nothing)", "requested", "sub1: (Finished (Right 1))"]

      pure unit

    it "only requests once for two subscriptions" do
      log <- liftEffect $ ERef.new []

      let request = liftEffect $ append log "requested"
      let d = mapAsync Async (pure request)

      _ <- execCleanupT do
        subscribeDyn_ (append log <<< ("sub1: " <> _) <<< show) d
        subscribeDyn_ (append log <<< ("sub2: " <> _) <<< show) d

      yieldAff

      log `shouldHaveValue`
        [ "sub1: (InProgress Nothing)"
        , "sub2: (InProgress Nothing)"
        , "requested"
        , "sub1: (Finished (Right unit))"
        , "sub2: (Finished (Right unit))"
        ]

      pure unit

    it "makes a request when the value changes" do
      avar <- AVar.empty
      log <- liftEffect $ ERef.new []

      r <- Ref.new (Sync 1)
      let result = mapAsync identity (Ref.value r)

      _ <- execCleanupT do
        subscribeDyn_ (append log <<< show) result

      log `shouldHaveValue` [ "(Finished (Right 1))" ]

      clear log
      Ref.write r $ Async do
        append log "requested"
        AVar.take avar
      yieldAff
      log `shouldHaveValue` [ "(InProgress (Just (Right 1)))", "requested" ]

      clear log
      AVar.put 2 avar
      log `shouldHaveValue` [ "(Finished (Right 2))" ]

    it "ignores responses to requests older than the current" do
      avar1 <- AVar.empty
      avar2 <- AVar.empty
      log <- liftEffect $ ERef.new []

      r <- Ref.new (Sync 1)
      let result = mapAsync identity (Ref.value r)

      _ <- execCleanupT do
        subscribeDyn_ (append log <<< show) result

      log `shouldHaveValue` [ "(Finished (Right 1))" ]

      clear log
      Ref.write r $ Async $ AVar.take avar1
      yieldAff
      Ref.write r $ Async $ AVar.take avar2
      yieldAff

      log `shouldHaveValue`
        [ "(InProgress (Just (Right 1)))"
        , "(InProgress (Just (Right 1)))"
        ]

      clear log
      AVar.put 11 avar1
      yieldAff
      log `shouldHaveValue` [] -- should be ignored, as new request is going on

      clear log
      AVar.put 12 avar2
      yieldAff
      log `shouldHaveValue` [ "(Finished (Right 12))" ]

    it "ignores out-of-order responses" do
      avar1 <- AVar.empty
      avar2 <- AVar.empty
      log <- liftEffect $ ERef.new []

      r <- Ref.new (Sync 1)
      let result = mapAsync identity (Ref.value r)

      _ <- execCleanupT do
        subscribeDyn_ (append log <<< show) result

      Ref.write r $ Async $ AVar.take avar1
      yieldAff
      Ref.write r $ Async $ AVar.take avar2
      yieldAff

      clear log
      AVar.put 12 avar2
      yieldAff
      log `shouldHaveValue` [ "(Finished (Right 12))" ]

      clear log
      AVar.put 11 avar1
      yieldAff
      log `shouldHaveValue` [] -- should be ignored, as this request was replaced by avar2

    it "request dynamic and status dynamic are consistent" do
      -- `let status = mapAsync identity request`
      -- Some relations must hold between the values of `status` and `request`:
      --
      -- - If `request` is `Sync x`, then `status` is immediately `Finished (Right x)`
      -- - If `request` is `Just x`, then `status` is either `Loading` or `Loaded y`,
      --   where `y` is the result of running `x`.
      --
      -- A naive implementation would expose intermediate states where these
      -- invariants don't hold. This test checks for this.

      avar <- AVar.empty

      -- In `r` we'll store pairs of (String, AsyncComputation Int).
      -- The first string is a description, and goes to the log;
      -- the action is the request.
      r <- Ref.new $ Tuple "Sync 1" (Sync 1 :: AsyncComputation Int)

      -- In `log` we'll have pairs of (String, String)
      -- The first String is the request description, the second is the result.
      log <- liftEffect $ ERef.new []

      let status = mapAsync snd (Ref.value r)
      let result = Tuple <$> map fst (Ref.value r) <*> (show <$> status)

      _ <- runCleanupT do
        subscribeDyn_ (append log) result

      log `shouldHaveValue` [ Tuple "Sync 1" "(Finished (Right 1))" ]
      readDynamic result `shouldReturn` Tuple "Sync 1" "(Finished (Right 1))"

      -- Test with asynchronous action
      clear log
      Ref.write r $ Tuple "async 2" $ Async $ AVar.take avar
      yieldAff
      log `shouldHaveValue` [ Tuple "async 2" "(InProgress (Just (Right 1)))" ]
      readDynamic result `shouldReturn` Tuple "async 2" "(InProgress (Just (Right 1)))"

      clear log
      AVar.put 2 avar
      yieldAff
      log `shouldHaveValue` [ Tuple "async 2" "(Finished (Right 2))" ]
      readDynamic result `shouldReturn` Tuple "async 2" "(Finished (Right 2))"

      -- Test with change to Sync
      clear log
      Ref.write r $ Tuple "Sync 3" (Sync 3)
      yieldAff
      log `shouldHaveValue` [ Tuple "Sync 3" "(Finished (Right 3))" ]
      readDynamic result `shouldReturn` Tuple "Sync 3" "(Finished (Right 3))"
