module FizzBuzz where

buildList :: [String] -> Int -> [String]
buildList [] _ = []
buildList (x:xs) n = value : buildList xs (n+1)
  where value = if x == ""
                then (show n)
                else x

fizzbuzz :: Int -> [String]
fizzbuzz n = take n $ buildList pattern 1
  where fizz = cycle ["","","Fizz"]
        buzz = cycle ["","","","","Buzz"]
        pattern = zipWith (++) fizz buzz
