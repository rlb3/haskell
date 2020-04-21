module Main where
  
import Control.Monad
import System.IO

fileName = "/Users/robert/test.md"

x = liftM lines $ readFile fileName

main = readFile fileName

