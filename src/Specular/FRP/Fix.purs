module Specular.FRP.Fix (
    fixDyn
  , fixEvent
  , class FixFRP
  , fixFRP
  , class FixFRPRecord
  , fixRecord
) where

import Prelude

import Control.Monad.Cleanup (class MonadCleanup)
import Control.Monad.IOSync.Class (class MonadIOSync, liftIOSync)
import Data.Record (delete, get, insert)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), snd)
import Specular.FRP.Base (class MonadFRP, Dynamic, Event, newEvent, subscribeDyn_, subscribeEvent_)
import Specular.FRP.WeakDynamic (WeakDynamic, holdWeakDyn)
import Type.Equality (class TypeEquals, to)
import Type.Prelude (class IsSymbol, class RowLacks)
import Type.Row (class RowToList, Cons, Nil, RLProxy(..))

fixEvent ::
     forall m a b
   . MonadFRP m
  => (Event a -> m (Tuple (Event a) b))
  -> m b
fixEvent f = do
  {event,fire} <- liftIOSync newEvent
  Tuple event' result <- f event
  subscribeEvent_ fire event'
  pure result

fixDyn ::
     forall m a b
   . MonadFRP m
  => (WeakDynamic a -> m (Tuple (Dynamic a) b))
  -> m b
fixDyn f = do
  {event,fire} <- liftIOSync newEvent
  wdyn <- holdWeakDyn event
  Tuple dyn result <- f wdyn
  subscribeDyn_ fire dyn
  pure result

class FixFRP input output | output -> input, input -> output where
  fixFRP :: forall m b. MonadFRP m => (input -> m (Tuple output b)) -> m b

instance fixFRPEvent :: FixFRP (Event a) (Event a) where
  fixFRP = fixEvent

instance fixFRPDynamic :: FixFRP (WeakDynamic a) (Dynamic a) where
  fixFRP = fixDyn

instance fixFRPRecord :: (FixFRPRecord ro_list ri ro, RowToList ro ro_list) => FixFRP (Record ri) (Record ro) where
  fixFRP = fixRecord (RLProxy :: RLProxy ro_list)

class FixFRPRecord ro_list ri ro | ro_list -> ri ro where
  fixRecord :: forall m b
     . MonadFRP m
    => RLProxy ro_list
    -> (Record ri -> m (Tuple (Record ro) b))
    -> m b

instance fixFRPRecordNil :: TypeEquals {} (Record empty) => FixFRPRecord Nil empty empty where
  fixRecord _ f = snd <$> f (to {})

instance fixFRPRecordCons ::
    ( IsSymbol label
    , RowLacks label tail_ri
    , RowCons label input tail_ri ri
    , RowLacks label tail_ro
    , RowCons label output tail_ro ro
    , FixFRP input output
    , FixFRPRecord tail_ro_list tail_ri tail_ro
    ) => FixFRPRecord (Cons label output tail_ro_list) ri ro where
  fixRecord _ f =
    fixFRP $ \(input :: input) ->
      fixRecord (RLProxy :: RLProxy tail_ro_list) $ \(tail_ri :: Record tail_ri) -> do
        let ri = insert (SProxy :: SProxy label) input tail_ri :: Record ri
        Tuple (ro :: Record ro) result <- f ri
        let output = get (SProxy :: SProxy label) ro :: output
        let tail_ro = delete (SProxy :: SProxy label) ro :: Record tail_ro
        pure (Tuple tail_ro (Tuple output result))
