module S where

import Control.Monad
import Control.Monad.State
import Control.Monad.State.Class

example :: State (Integer, Integer) Integer
example = do
  replicateM 100 $ do
    (a,b) <- get
    put (a+1, b)
  (a, _) <- get
  return a

x = fst $ runState example (0, 1)
