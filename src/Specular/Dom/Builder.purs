module Specular.Dom.Builder (
    Builder
  , runBuilder

  , dynamic_

  , el
  , elAttr
  , elDynAttr'
  , elDynAttr
  , text
  , dynText
  , domEventWithSample
) where

import Prelude

import Control.Monad.Cleanup (class MonadCleanup, onCleanup)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.IOSync (IOSync)
import Control.Monad.IOSync.Class (class MonadIOSync, liftIOSync)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Replace (class MonadReplace, runReplaceable)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Array as A
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.StrMap as SM
import Data.Tuple (Tuple(..), snd)
import Specular.Dom.Node.Class (class DOM, class EventDOM, Attrs, EventType, addEventListener, appendChild, createDocumentFragment, createElement, createTextNode, insertBefore, parentNode, removeAllBetween, removeAttributes, setAttributes)
import Specular.FRP (Dynamic, Event, newEvent, subscribeDyn_)

newtype Builder node a = Builder (ReaderT (BuilderEnv node) (WriterT (IOSync Unit) IOSync) a)

derive newtype instance functorBuilder :: Functor (Builder node)
derive newtype instance applyBuilder :: Apply (Builder node)
derive newtype instance applicativeBuilder :: Applicative (Builder node)
derive newtype instance bindBuilder :: Bind (Builder node)
derive newtype instance monadBuilder :: Monad (Builder node)
derive newtype instance monadEffBuilder :: MonadEff eff (Builder node)
derive newtype instance monadIOSyncBuilder :: MonadIOSync (Builder node)

unBuilder :: forall node a. Builder node a -> ReaderT (BuilderEnv node) (WriterT (IOSync Unit) IOSync) a
unBuilder (Builder f) = f

runBuilder :: forall node a. BuilderEnv node -> Builder node a -> IOSync (Tuple a (IOSync Unit))
runBuilder env (Builder f) = runWriterT $ runReaderT f env

type BuilderEnv node = { parent :: node }

getEnv :: forall node. Builder node (BuilderEnv node)
getEnv = Builder ask

setParent :: forall node. node -> BuilderEnv node -> BuilderEnv node
setParent parent env = env { parent = parent }

instance monadCleanupBuilder :: MonadCleanup (Builder node) where
  onCleanup action = Builder $ tell action

instance monadReplaceBuilder :: DOM node => MonadReplace (Builder node) where
  runReplaceable initial = do
    env <- getEnv

    placeholderAfter <- liftIOSync $ createTextNode ""
    liftIOSync $ appendChild placeholderAfter env.parent

    cleanupRef <- liftIOSync $ newIORef mempty

    let
      replaceWith inner = do
        join $ readIORef cleanupRef

        fragment <- createDocumentFragment
        Tuple _ innerCleanup <- runBuilder { parent: fragment } inner

        m_parent <- parentNode placeholderAfter

        case m_parent of
          Just parent -> do
            placeholderBefore <- createTextNode ""
            insertBefore placeholderBefore placeholderAfter parent
            insertBefore fragment placeholderAfter parent

            writeIORef cleanupRef $ do
              innerCleanup
              removeAllBetween placeholderBefore placeholderAfter
              writeIORef cleanupRef mempty -- TODO: explain this

          Nothing ->
            -- we've been removed from the DOM
            writeIORef cleanupRef innerCleanup

    onCleanup $ join $ readIORef cleanupRef

    pure { replace: replaceWith }

dynamic_ :: forall m. MonadReplace m => MonadIOSync m => Dynamic (m Unit) -> m Unit
dynamic_ dyn = do
  {replace} <- runReplaceable (pure unit)
  subscribeDyn_ replace dyn

elDynAttr' ::
     forall node a
   . DOM node
  => String
  -> Dynamic Attrs
  -> Builder node a
  -> Builder node (Tuple node a)
elDynAttr' tagName dynAttrs inner = do
  env <- getEnv
  node <- liftIOSync $ createElement tagName

  attrsRef <- liftIOSync $ newIORef mempty
  let
    resetAttributes newAttrs = do
      oldAttrs <- readIORef attrsRef
      writeIORef attrsRef newAttrs
      let
        changed = SM.filterWithKey (\k v -> SM.lookup k oldAttrs /= Just v) newAttrs
        removed = A.filter (\k -> not (k `SM.member` newAttrs)) $ SM.keys oldAttrs

      removeAttributes node removed
      setAttributes node changed

  subscribeDyn_ resetAttributes dynAttrs
  result <- Builder $ local (setParent node) (unBuilder inner)
  liftIOSync $ appendChild node env.parent
  pure (Tuple node result)

elDynAttr ::
     forall node a
   . DOM node
  => String
  -> Dynamic Attrs
  -> Builder node a
  -> Builder node a
elDynAttr tagName dynAttrs inner = snd <$> elDynAttr' tagName dynAttrs inner

elAttr ::
     forall node a
   . DOM node
  => String
  -> Attrs
  -> Builder node a
  -> Builder node a
elAttr tagName attrs inner = elDynAttr tagName (pure attrs) inner

el ::
     forall node a
   . DOM node
  => String
  -> Builder node a
  -> Builder node a
el tagName inner = elAttr tagName mempty inner

text :: forall node. DOM node => String -> Builder node Unit
text str = do
  env <- getEnv
  liftIOSync $ do
    node <- createTextNode str
    appendChild node env.parent

dynText :: forall node. DOM node => Dynamic String -> Builder node Unit
dynText = dynamic_ <<< map text

domEventWithSample ::
     forall event node m a
   . EventDOM event node
  => MonadIOSync m
  => MonadCleanup m
  => (event -> IOSync a)
  -> EventType
  -> node
  -> m (Event a)
domEventWithSample sample eventType node = do
  {event,fire} <- liftIOSync newEvent
  unsub <- liftIOSync $ addEventListener eventType (sample >=> fire) node
  onCleanup unsub
  pure event
