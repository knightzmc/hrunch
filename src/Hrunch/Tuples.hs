module Hrunch.Tuples where

mapFirst :: (a, b) -> (a -> c) -> (c, b)
mapFirst (a, b) f = (f a, b)

infixl 4 <$$>

(<$$>) :: (a -> c) -> (a, b) -> (c, b)
(<$$>) = flip mapFirst
