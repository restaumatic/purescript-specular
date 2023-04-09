module DynamicSpec where

import Prelude hiding (append)

import Control.Monad.Cleanup (execCleanupT, runCleanupT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Debug (traceM)
import Effect.Console (log) as Console
import Effect.Ref (new)
import Specular.FRP (Dynamic, annotated, foldDyn, foldDynMaybe, holdDyn, holdUniqDynBy, newEvent, readDynamic, subscribeDyn_, subscribeEvent_)
import Specular.FRP.Base (changed, latestJust, newDynamic, subscribeDyn)
import Specular.Ref as Ref
import Test.Spec (Spec, describe, it, pending')
import Test.Utils (append, clear, liftEffect, shouldHaveValue, shouldReturn, withLeakCheck, withLeakCheck')

spec :: Spec Unit
spec = describe "Dynamic" $ do

  describe "holdDyn" $ do
    it "updates value when someone is subscribed to changes" $ withLeakCheck $ do
      { event, fire } <- liftEffect newEvent
      log <- liftEffect $ new []
      Tuple dyn unsub1 <- liftEffect $ runCleanupT $ holdDyn 0 event

      unsub <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn
      log `shouldHaveValue` [ 0 ]

      clear log
      liftEffect $ fire 1
      liftEffect unsub
      liftEffect $ fire 2

      log `shouldHaveValue` [ 1 ]

      -- clean up
      liftEffect unsub1

    it "updates value when no one is subscribed" $ withLeakCheck $ do
      { event, fire } <- liftEffect newEvent
      log <- liftEffect $ new []
      Tuple dyn unsub1 <- liftEffect $ runCleanupT $ holdDyn 0 event

      liftEffect $ fire 2

      unsub2 <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn

      log `shouldHaveValue` [ 2 ]

      -- clean up
      liftEffect unsub1
      liftEffect unsub2

  describe "newDynamic" $ do
    it "updates value when someone is subscribed to changes" $ withLeakCheck $ do
      { dynamic: dyn, read, set } <- liftEffect $ newDynamic 0
      log <- liftEffect $ new []

      unsub <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn
      log `shouldHaveValue` [ 0 ]

      clear log
      liftEffect $ set 1
      liftEffect unsub
      liftEffect $ set 2

      log `shouldHaveValue` [ 1 ]

      liftEffect read `shouldReturn` 2

    it "updates value when no one is subscribed" $ withLeakCheck $ do
      { dynamic: dyn, read, set } <- liftEffect $ newDynamic 0
      log <- liftEffect $ new []

      liftEffect $ set 2

      unsub <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn

      log `shouldHaveValue` [ 2 ]
      liftEffect read `shouldReturn` 2

      -- clean up
      liftEffect unsub

  describe "holdUniqDynBy" $ do
    it "updates value only when it changes" $ withLeakCheck $ do
      { event, fire } <- liftEffect newEvent
      log <- liftEffect $ new []
      Tuple dyn unsub1 <- liftEffect $ runCleanupT $ holdUniqDynBy eq 0 event

      unsub2 <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn
      log `shouldHaveValue` [ 0 ]

      clear log
      liftEffect $ fire 0
      liftEffect $ fire 1
      liftEffect $ fire 2
      liftEffect $ fire 2

      log `shouldHaveValue` [ 1, 2 ]

      -- clean up
      liftEffect unsub1
      liftEffect unsub2

  describe "foldDyn" $ do
    it "updates value correctly" $ withLeakCheck $ do
      { event, fire } <- liftEffect newEvent
      log <- liftEffect $ new []
      Tuple dyn unsub1 <- liftEffect $ runCleanupT $ foldDyn add 0 event

      unsub2 <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn

      liftEffect $ fire 1
      liftEffect $ fire 2
      liftEffect $ fire 3

      log `shouldHaveValue` [ 0, 1, 3, 6 ]

      -- clean up
      liftEffect unsub1
      liftEffect unsub2

    it "doesn't double-update in the presence of binds" $ withLeakCheck $ do
      { event, fire } <- liftEffect newEvent
      log <- liftEffect $ new []
      Tuple dyn unsub1 <- liftEffect $ runCleanupT $ foldDyn add 0 event

      unsub2 <- liftEffect $ execCleanupT do
        let
          dyn2 = do
            _ <- dyn
            _ <- dyn
            dyn
        subscribeDyn_ (append log) dyn2

      liftEffect $ fire 1

      log `shouldHaveValue` [ 0, 1 ]
      liftEffect (readDynamic dyn) `shouldReturn` 1

      -- clean up
      liftEffect unsub1
      liftEffect unsub2

  describe "foldDynMaybe" $ do
    it "triggers only when function returns Just" $ withLeakCheck $ do
      { event, fire } <- liftEffect newEvent
      log <- liftEffect $ new []
      Tuple dyn unsub1 <- liftEffect $ runCleanupT $ foldDynMaybe (\x y -> (_ + y) <$> x) 1 event

      unsub2 <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn

      liftEffect $ fire Nothing
      liftEffect $ fire (Just 1)
      liftEffect $ fire Nothing
      liftEffect $ fire (Just 2)
      liftEffect $ fire Nothing
      liftEffect $ fire (Just 3)
      liftEffect $ fire Nothing

      log `shouldHaveValue` [ 1, 2, 4, 7 ]

      -- clean up
      liftEffect unsub1
      liftEffect unsub2

  describe "Applicative instance" $ do
    it "works with different root Dynamics" $ withLeakCheck $ do
      ev1 <- liftEffect newEvent
      ev2 <- liftEffect newEvent
      log <- liftEffect $ new []
      Tuple rootDyn1 unsub1 <- liftEffect $ runCleanupT $ holdDyn 0 ev1.event
      Tuple rootDyn2 unsub2 <- liftEffect $ runCleanupT $ holdDyn 10 ev2.event

      let dyn = Tuple <$> rootDyn1 <*> rootDyn2
      unsub3 <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn

      liftEffect $ ev1.fire 1
      log `shouldHaveValue` [ Tuple 0 10, Tuple 1 10 ]

      clear log
      liftEffect $ ev2.fire 5
      log `shouldHaveValue` [ Tuple 1 5 ]

      -- clean up
      liftEffect unsub1
      liftEffect unsub2
      liftEffect unsub3

    it "has no glitches when used with the same root Dynamic" $ withLeakCheck $ do
      { event, fire } <- liftEffect newEvent
      log <- liftEffect $ new []
      Tuple rootDyn unsub1 <- liftEffect $ runCleanupT $ holdDyn 0 event

      let dyn = Tuple <$> rootDyn <*> (map (_ + 10) rootDyn)
      unsub2 <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn

      liftEffect $ fire 1

      log `shouldHaveValue` [ Tuple 0 10, Tuple 1 11 ]

      -- clean up
      liftEffect unsub1
      liftEffect unsub2

  describe "Monad instance" $ do
    it "works" $ withLeakCheck $ do
      log <- liftEffect $ new []
      rootDynInner <- Ref.new 0
      rootDynOuter <- Ref.new $ annotated "rootDynInner" $ Ref.value rootDynInner

      let dyn = join $ annotated "rootDynOuter" $ Ref.value rootDynOuter
      unsub3 <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn
      log `shouldHaveValue` [ 0 ]
      clear log

      -- inner fires
      Ref.write rootDynInner 1
      log `shouldHaveValue` [ 1 ]
      clear log

      -- outer fires
      Ref.write rootDynOuter (pure 2)
      log `shouldHaveValue` [ 2 ]
      clear log


      -- inner fires when outer not pointing to it
      Ref.write rootDynInner 10
      log `shouldHaveValue` []

      -- outer fires to itself
      Ref.write rootDynOuter (3 <$ Ref.value rootDynOuter)
      log `shouldHaveValue` [ 3 ]
      clear log

      -- outer fires to itself again
      Ref.write rootDynOuter (4 <$ Ref.value rootDynOuter)
      log `shouldHaveValue` [ 4 ]
      clear log

      -- outer fires to inner
      Ref.write rootDynOuter (Ref.value rootDynInner)
      log `shouldHaveValue` [ 10 ]
      clear log

      -- extra subscription should not mess things up
      unsub4 <- liftEffect $ execCleanupT $ subscribeDyn_ (\_ -> pure unit) dyn
      Ref.write rootDynInner 15
      Ref.write rootDynOuter (Ref.value rootDynInner)
      log `shouldHaveValue` [ 15, 15 ]

      -- clean up
      liftEffect unsub3
      liftEffect unsub4

    it "triple bind with the same root" $ withLeakCheck $ do
      ev <- liftEffect newEvent
      unsub1 <- liftEffect $ execCleanupT $ do
        rootDyn <- holdDyn unit ev.event
        let
          dyn = do
            rootDyn
            rootDyn
            rootDyn

        subscribeDyn_ (\_ -> pure unit) dyn

      withLeakCheck' "first fire" $ liftEffect $ ev.fire unit
      withLeakCheck' "second fire" $ liftEffect $ ev.fire unit

      -- clean up
      liftEffect unsub1

    it "should not mess up event delivery order" $ withLeakCheck do
      log <- liftEffect $ new []
      rootDynOuter <- liftEffect $ newDynamic { set: \_ -> pure unit, modify: \_ -> pure unit, read: pure 0, dynamic: pure 0 }

      let
        dyn :: Dynamic Int
        dyn = rootDynOuter.dynamic >>= _.dynamic
      unsub3 <- liftEffect $ execCleanupT do
        flip subscribeDyn_ dyn \x -> do
          append log $ Right $ "receiver 1: " <> show x
        flip subscribeDyn_ rootDynOuter.dynamic \x -> do
          value <- x.read
          when (value > 0) do
            append log $ Right "set begin"
            x.set (value + 1)
            append log $ Right "set end"
        flip subscribeDyn_ dyn \x -> do
          -- | traceM $ "receiver 2: " <> show x
          append log $ Left x

      liftEffect do
        inner <- newDynamic 1
        rootDynOuter.set inner

      log `shouldHaveValue` [ (Right "receiver 1: 0"), (Left 0), (Right "set begin"), (Right "receiver 1: 1"), (Left 1), (Right "receiver 1: 2"), (Left 2), (Right "set end") ]

      -- clean up
      liftEffect unsub3

    pending' "weird glitch test" $ withLeakCheck do
      -- Example minimized from a real-world bug.
      log <- liftEffect $ new []
      root <- Ref.new "1"
      unsub1 <- liftEffect $ execCleanupT do
        let
          dyn = ado
            x <-
              pure unit >>= \_ ->
                ((identity <$> pure unit) *> Ref.value root) >>= \x ->
                  pure x
            y <- Ref.value root
            in [ x, y ]
        subscribeEvent_ (append log) (changed dyn)

      liftEffect $ Ref.write root "2"
      liftEffect $ Ref.write root "3"

      log `shouldHaveValue`
        [ [ "2", "2" ]
        , [ "3", "3" ]
        ]

      -- clean up
      liftEffect unsub1

  describe "subscribeDyn_" $ do
    it "simple case - no changes" $ withLeakCheck $ do
      log <- liftEffect $ new ([] :: Array (Either Int Int))
      { dynamic: dyn, set: _fire } <- newDynamic 1

      Tuple _derivedDyn unsub2 <- liftEffect $ runCleanupT $ subscribeDyn_
        ( \x ->
            do
              append log (Left x)
        )
        dyn

      log `shouldHaveValue` [ Left 1 ]

      -- clean up
      liftEffect unsub2

  describe "subscribeDyn" $ do
    it "simple case - no changes" $ withLeakCheck $ do
      log <- liftEffect $ new ([] :: Array (Either Int Int))
      { dynamic: dyn, set: _fire } <- newDynamic 1

      Tuple _derivedDyn unsub2 <- liftEffect $ runCleanupT $ subscribeDyn
        ( \x ->
            do
              append log (Left x)
              pure (2 * x)
        )
        dyn

      log `shouldHaveValue` [ Left 1 ]

      -- clean up
      liftEffect unsub2

    it "updates the resulting Dynamic" $ withLeakCheck $ do
      { event, fire } <- liftEffect newEvent
      log <- liftEffect $ new []
      Tuple dyn unsub1 <- liftEffect $ runCleanupT $ holdDyn 1 event

      Tuple derivedDyn unsub2 <- liftEffect $ runCleanupT $ subscribeDyn
        ( \x ->
            do
              append log (Left x)
              pure (2 * x)
        )
        dyn

      unsub3 <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log (Right x)) derivedDyn

      liftEffect $ fire 5

      log `shouldHaveValue` [ Left 1, Right 2, Left 5, Right 10 ]

      -- clean up
      liftEffect unsub1
      liftEffect unsub2
      liftEffect unsub3

  describe "latestJust" $ do
    it "updates value only when it changes to Just" $ withLeakCheck $ do
      { event, fire } <- liftEffect newEvent
      let
        fire' x = liftEffect do
          Console.log $ "fire " <> show x
          fire x
      log <- liftEffect $ new []
      Tuple dyn unsub1 <- liftEffect $ runCleanupT $ holdDyn Nothing event >>= latestJust

      unsub2 <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn

      fire' Nothing
      fire' (Just 1)
      fire' Nothing
      fire' (Just 2)

      log `shouldHaveValue` [ Nothing, Just 1, Just 2 ]

      -- clean up
      liftEffect unsub1
      liftEffect unsub2
