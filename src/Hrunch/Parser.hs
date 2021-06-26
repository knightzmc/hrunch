{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Hrunch.Parser where

import Hrunch.Lexer

data ParseError = ParseError
  { parseErrorType :: ParseErrorType,
    cause :: [Token]
  }
  deriving (Show, Eq)

data ParseErrorType
  = UnbalancedParens
  | NoInput
  | NotEnoughArguments
  deriving (Show, Eq)

newtype Parser a = Parser {parse :: [Token] -> [(Either ParseError a, [Token])]}

unit :: a -> Parser a
unit a = Parser $ \s -> [(Right a, s)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \tokens -> do
  let initialParsed = parse p tokens
  concatMap flatten initialParsed
  where
    flatten (resEither, remainingTokens) = case resEither of
      Left err -> [(Left err, remainingTokens)]
      Right a -> parse (f a) remainingTokens

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p@(Parser _) = Parser $ \tokens -> map f' $ parse p tokens
    where
      f' (resEither, remainingTokens) = case resEither of
        Left err -> (Left err, remainingTokens)
        Right a -> (Right $ f a, remainingTokens)

item :: Parser Token
item = Parser $ \case
  [] -> []
  (c : cs) -> [(Right c, cs)]

satisfy :: (Token -> Bool) -> Parser Token
satisfy p =
  item `bind` \c ->
    if p c
      then unit c
      else Parser (const [])
