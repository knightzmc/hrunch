import Control.Monad
import Hrunch.Expressions
import Test.HUnit

main :: IO ()
main = void $ runTestTT tests

tests :: Test
tests =
  TestList
    [ TestLabel "testConstantExpression" testConstantExpression,
      TestLabel "testAdditionExpression" testAdditionExpression,
      testDivisionExpression,
      testDivisionByZeroExpression
    ]

testConstantExpression :: Test
testConstantExpression = TestCase $ assertEqual "for constant expr" (Right 3.0) (evaluate emptyContext $ ConstantExpression 3.0)

testAdditionExpression :: Test
testAdditionExpression =
  TestCase $
    assertEqual "adding 3 + 4" (Right 7.0) (evaluate emptyContext $ AddExpression (ConstantExpression 3.0) (ConstantExpression 4.0))

testDivisionExpression :: Test
testDivisionExpression =
  TestCase $
    assertEqual "dividing 10 / 2" (Right 5.0) (evaluate emptyContext $ DivideExpression (ConstantExpression 10.0) (ConstantExpression 2.0))

testDivisionByZeroExpression :: Test
testDivisionByZeroExpression =
  let divisionExpr = DivideExpression (ConstantExpression 10.0) (ConstantExpression 0.0) in
  TestCase $
    assertEqual "dividing 10 / 0"
      (Left $ EvaluationError DivisionByZero divisionExpr)
      (evaluate emptyContext divisionExpr)

