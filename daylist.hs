module DayList where

import Data.List

data Day = Sun | Mon | Tues | Wed | Thurs | Fri | Sat deriving (Show, Eq, Ord, Read)

data InitDayList = Init Day Day deriving Show

days :: [Day]
days = [Sun, Mon, Tues, Wed, Thurs, Fri, Sat]

daylist :: InitDayList -> [Day] -> [Day]
daylist (Init start stop) days =
  daylist_helper start stop days []

daylist_helper :: Day -> Day -> [Day] -> [Day] -> [Day]
daylist_helper _ _ [] acc = acc
daylist_helper start stop (d:ds) acc
  | any (start==) acc = daylist_helper start stop ds (d:acc)  
  | start == d = daylist_helper start stop ds (d:acc)
  | stop == d = reverse acc ++ [d]
  | ds == [] = daylist_helper stop start days (d:acc)
  | otherwise = daylist_helper start stop ds acc
