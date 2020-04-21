module Clock where

data Clock = Time Int | Hour Int deriving (Show)

instance  Monoid Clock  where
  mempty = Hour 12
  mappend (Time a) (Hour b)
    | a + b > 12  = Time ((a + b) `div` 12)
    | otherwise = Time (a + b)

t = Time 10
h = Hour 4



