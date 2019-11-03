module Test.ModelMain where

import Prelude

import Control.Lazy (defer)
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
        Console.log $ show program
        pure $ Success -- Failure $ pure $ show program
  if not b then unsafeCrashWith "Failure" else pure unit

type Value = String
type Time = Int

type SemDynamic a = List (Tuple Time a)

data Program
  = NewDynamic (SemDynamic Value) Program
  | Return Expr_Dynamic

derive instance genericProgram :: Generic Program _
instance showProgram :: Show Program where show x = genericShow x

data Expr_Dynamic
  = Var Int
  | Pure Expr_Value
  | Map Expr_Value Expr_Dynamic
  | Map2 Expr_Value Expr_Dynamic Expr_Dynamic

derive instance genericExpr_Dynamic :: Generic Expr_Dynamic _
instance showExpr_Dynamic :: Show Expr_Dynamic where show x = genericShow x

data Expr_Value
  = ConstantValue Value
  | ApplyValue Expr_Value Expr_Value

derive instance genericExpr_Value :: Generic Expr_Value _
instance showExpr_Value :: Show Expr_Value where show x = genericShow x

type Env = Int -- number of variables available

emptyEnv :: Env
emptyEnv = 0

genProgram :: Env -> Gen Program
genProgram env = oneOfRec
  [ Return <$> genExprDynamic env ]
  [ defer \_ -> NewDynamic <$> genSemDynamic <*> genProgram (env + 1) ]

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

genValue :: Gen Value
genValue = oneOf $ map pure [ "x", "y", "z" ]

genExprDynamic :: Env -> Gen Expr_Dynamic
genExprDynamic env = oneOfRec
  ((if env > 0 then [ Var <$> chooseInt 0 (env-1) ] else []) <>
  [ Pure <$> genExprValue
  ])
  []

genExprValue :: Gen Expr_Value
genExprValue = oneOfRec
  [ ConstantValue <$> genValue ]
  [ defer \_ -> ApplyValue <$> genExprValue <*> genExprValue ]
