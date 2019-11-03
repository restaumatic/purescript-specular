module Test.ModelMain where

import Prelude

import Control.Lazy (defer)
import Data.Array (fromFoldable) as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log) as Console
import Jack (Result(..), checkM, chooseInt, forAll, oneOf, oneOfRec, resultM)
import Jack.Gen (Gen)
import Partial.Unsafe (unsafeCrashWith)

main :: Effect Unit
main = do
  b <- checkM do
    forAll (genProgram 0) \program -> do
      resultM do
        Console.log "--------------------------------------------------------------------------------"
        Console.log $ ppProgram program
        pure $ Success -- Failure $ pure $ show program
  if not b then unsafeCrashWith "Failure" else pure unit

type Value = String
type Fn = String -- uninterpreted function
type Time = Int
type VarId = Int

type SemDynamic a = List (Tuple Time a)

genSemDynamic :: Gen (SemDynamic Value)
genSemDynamic = go 0
  where
    go t = do
      value <- genValue
      next <- oneOfRec
        [ pure Nil ]
        [ do timestep <- chooseInt 1 5
             go (t + timestep)
        ]
      pure (Cons (Tuple t value) next)

data Program
  = NewDynamic Int (SemDynamic Value) Program
  | Return Expr_Dynamic

derive instance genericProgram :: Generic Program _
instance showProgram :: Show Program where show x = genericShow x

genProgram :: Env -> Gen Program
genProgram env = oneOfRec
  [ Return <$> genExprDynamic env ]
  [ defer \_ -> NewDynamic env <$> genSemDynamic <*> genProgram (env + 1) ]

ppProgram :: Program -> String
ppProgram =
  case _ of
    NewDynamic binder sem next ->
      "v" <> show binder <> " <- newDynamic -- " <> show (Array.fromFoldable sem) <> "\n" <>
      ppProgram next

    Return expr ->
      "pure (" <> ppExprDynamic expr <> ")"

{-
data Expr_Value
  = ConstantValue Value
  | ApplyValue Expr_Value Expr_Value

derive instance genericExpr_Value :: Generic Expr_Value _
instance showExpr_Value :: Show Expr_Value where show x = genericShow x
-}

type Env = Int -- number of variables available

emptyEnv :: Env
emptyEnv = 0

genValue :: Gen Value
genValue = oneOf $ map pure [ "a", "b", "c", "d", "x", "y", "z" ]

genFn :: Gen Fn
genFn = oneOf $ map pure [ "f", "g", "h" ]

data Expr_Dynamic
  = Var Int
  | Pure Value
  | Map Fn Expr_Dynamic
  | Map2 Fn Expr_Dynamic Expr_Dynamic

derive instance genericExpr_Dynamic :: Generic Expr_Dynamic _
instance showExpr_Dynamic :: Show Expr_Dynamic where show x = genericShow x

genExprDynamic :: Env -> Gen Expr_Dynamic
genExprDynamic env = oneOfRec
  ((if env > 0 then [ Var <$> chooseInt 0 (env-1) ] else []) <>
  [ Pure <$> genValue
  ])
  [ defer \_ -> Map <$> genFn <*> genExprDynamic env
  , defer \_ -> Map2 <$> genFn <*> genExprDynamic env <*> genExprDynamic env
  ]

ppExprDynamic :: Expr_Dynamic -> String
ppExprDynamic =
  case _ of
    Var n -> "v" <> show n
    Pure v -> "pure " <> v
    Map f d -> "map " <> f <> " (" <> ppExprDynamic d <> ")"
    Map2 f d1 d2 -> "map2 " <> f <> " (" <> ppExprDynamic d1 <> ") (" <> ppExprDynamic d2 <> ")"
