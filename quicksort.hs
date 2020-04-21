module QuickSort where

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort lesser ++ [x] ++ quicksort greater
  where lesser  = filter (\y -> x > y) xs
        greater = filter (\y -> x < y) xs
