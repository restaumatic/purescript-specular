module Specular.FRP.Fix
  ( fixDyn
  , fixEvent
  , class FixFRP
  , fixFRP
  , fixFRP_
  , class FixFRPRecord
  , fixRecord
  ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy(..))
import Data.Tuple (Tuple(Tuple), snd)
import Prim.Row as Row
import Prim.RowList (RowList)
import Record (delete, get, insert)
import Specular.FRP.Base (class MonadFRP, Dynamic, Event, newEvent, subscribeDyn_, subscribeEvent_)
import Specular.FRP.WeakDynamic (WeakDynamic, holdWeakDyn)
import Type.Equality (class TypeEquals, to)
import Type.RowList (class RowToList, Cons, Nil)

fixEvent
  :: forall m a b
   . MonadFRP m
  => (Event a -> m (Tuple (Event a) b))
  -> m b
fixEvent f = do
  { event, fire } <- newEvent
  Tuple event' result <- f event
  subscribeEvent_ fire event'
  pure result

fixDyn
  :: forall m a b
   . MonadFRP m
  => (WeakDynamic a -> m (Tuple (Dynamic a) b))
  -> m b
fixDyn f = do
  { event, fire } <- newEvent
  wdyn <- holdWeakDyn event
  Tuple dyn result <- f wdyn
  subscribeDyn_ fire dyn
  pure result

fixFRP_ :: forall input output m. FixFRP input output => MonadFRP m => (input -> m output) -> m Unit
fixFRP_ f = fixFRP (\input -> (\x -> Tuple x unit) <$> f input)

class FixFRP input output | output -> input, input -> output where
  fixFRP :: forall m b. MonadFRP m => (input -> m (Tuple output b)) -> m b

instance fixFRPEvent :: FixFRP (Event a) (Event a) where
  fixFRP = fixEvent

instance fixFRPDynamic :: FixFRP (WeakDynamic a) (Dynamic a) where
  fixFRP = fixDyn

instance fixFRPRecord :: (FixFRPRecord ro_list ri ro, RowToList ro ro_list) => FixFRP (Record ri) (Record ro) where
  fixFRP = fixRecord (Proxy :: Proxy ro_list)

class FixFRPRecord (ro_list :: RowList Type) (ri :: Row Type) (ro :: Row Type) | ro_list -> ri ro where
  fixRecord
    :: forall m b
     . MonadFRP m
    => Proxy ro_list
    -> (Record ri -> m (Tuple (Record ro) b))
    -> m b

instance fixFRPRecordNil :: TypeEquals {} (Record empty) => FixFRPRecord Nil empty empty where
  fixRecord _ f = snd <$> f (to {})

instance fixFRPRecordCons ::
  ( IsSymbol label
  , Row.Lacks label tail_ri
  , Row.Cons label input tail_ri ri
  , Row.Lacks label tail_ro
  , Row.Cons label output tail_ro ro
  , FixFRP input output
  , FixFRPRecord tail_ro_list tail_ri tail_ro
  ) =>
  FixFRPRecord (Cons label output tail_ro_list) ri ro where
  fixRecord _ f =
    fixFRP $ \(input :: input) ->
      fixRecord (Proxy :: Proxy tail_ro_list) $ \(tail_ri :: Record tail_ri) -> do
        let ri = insert (Proxy :: Proxy label) input tail_ri :: Record ri
        Tuple (ro :: Record ro) result <- f ri
        let output = get (Proxy :: Proxy label) ro :: output
        let tail_ro = delete (Proxy :: Proxy label) ro :: Record tail_ro
        pure (Tuple tail_ro (Tuple output result))
