module Hrunch.Expressions where

import Control.Applicative (liftA2)
import Data.Either.Combinators
import qualified Data.Map as M

type VariableName = String

data Expression
  = VariableExpression VariableName
  | ConstantExpression Double
  | AddExpression Expression Expression
  | SubtractExpression Expression Expression
  | MultiplyExpression Expression Expression
  | DivideExpression Expression Expression
  deriving (Eq, Show)

type Function = ()

data Context = Context
  { variables :: M.Map VariableName Double,
    functions :: M.Map VariableName Function
  }

emptyContext :: Context
emptyContext =
  Context
    { variables = M.empty,
      functions = M.empty
    }

data EvaluationErrorType
  = DivisionByZero
  | UnboundVariableError
  deriving (Eq, Show)

data EvaluationError = EvaluationError
  { errorType :: EvaluationErrorType,
    cause :: Expression
  }
  deriving (Eq, Show)

class Evaluate t where
  evaluate :: Context -> t -> Either EvaluationError Double

binaryCombine :: (Evaluate t1, Evaluate t2) => (Double -> Double -> c) -> t1 -> t2 -> Context -> Either EvaluationError c
binaryCombine f left right context = liftA2 f (evaluate context left) (evaluate context right)

instance Evaluate Expression where
  evaluate context expr = case expr of
    (VariableExpression name) -> maybeToRight (EvaluationError UnboundVariableError expr) $ variables context M.!? name
    (ConstantExpression value) -> pure value
    (AddExpression left right) -> binaryCombine (+) left right context
    (SubtractExpression left right) -> binaryCombine (-) left right context
    (MultiplyExpression left right) -> binaryCombine (*) left right context
    (DivideExpression left right) -> do
      let divisor = evaluate context right
      case divisor of
        Right 0.0 -> Left $ EvaluationError DivisionByZero expr
        Right divisorValue -> binaryCombine (/) left (ConstantExpression divisorValue) context
        err -> err
