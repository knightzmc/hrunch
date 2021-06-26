module Hrunch.Lexer where

import Data.Char
import Data.Foldable (foldl')
import Hrunch.Tuples

data Token
  = Literal Double
  | Identifier String
  | LParen
  | RParen
  | Comma
  | Plus
  | Minus
  | Times
  | Divide
  | Exp
  deriving (Eq, Show)

lexExpression :: String -> [Token]
lexExpression [] = []
lexExpression (c : rest)
  | c == '+' = Plus : lexExpression rest
  | c == '-' = Minus : lexExpression rest
  | c == '*' = Times : lexExpression rest
  | c == '/' = Divide : lexExpression rest
  | c == '^' = Exp : lexExpression rest
  | c == '(' = LParen : lexExpression rest
  | c == ')' = RParen : lexExpression rest
  | c == ',' = Comma : lexExpression rest
  | c == ' ' = lexExpression rest -- Skip whitespace
  | isDigit c = let (value, other) = lexDigit (c : rest) in Literal value : lexExpression other
  | otherwise = case lexExpression rest of
    (Identifier s : others) -> Identifier (c : s) : others
    other -> Identifier [c] : other

charToInt :: Char -> Int
charToInt c = ord c - 48

charToDouble :: Char -> Double
charToDouble = fromIntegral . charToInt

lexDigit :: String -> (Double, String)
lexDigit numString = lexDigit' numString 0.0
  where
    lexDigit' :: String -> Double -> (Double, String)
    lexDigit' ('-' : str) acc = negate <$$> lexDigit' str acc
    lexDigit' [] acc = (acc, "")
    lexDigit' (c : str) acc
      | isDigit c = lexDigit' str (acc * 10 + charToDouble c)
      | c == '.' = (acc +) <$$> lexDecimalPart str
      | otherwise = (acc, c : str)

    lexDecimalPart :: String -> (Double, String)
    lexDecimalPart decimalStr =
      parseDec <$$> span isDigit decimalStr
      where
        parseDec s = foldl' (\acc val -> acc / 10.0 + val / 10.0) 0.0 $ map (fromIntegral . charToInt) $ reverse s
