module FizzBuzz where

fizz_or_buzz :: Int -> String
fizz_or_buzz n
  | n `mod` 3 == 0 && n `mod` 5 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = (show n)

fizzbuzz :: Int -> [String]
fizzbuzz n =
  map fizz_or_buzz [1..n]
