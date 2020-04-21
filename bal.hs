module Bal where

data B = B Int Int deriving (Eq, Show)

instance Monoid B where
  mempty = B 0 0
  mappend (B a b) (B c d)
    | b <= c = B (a + c - b) d
    | otherwise = B a (d + b - c)

parse '(' = B 0 1
parse ')' = B 1 0
parse _ = B 0 0

balanced xs = foldMap parse xs == B 0 0
