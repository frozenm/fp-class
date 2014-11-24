{-
  Пользуясь средствами монады ST, запрограммировать сортировку массива тремя любыми методами.
-}

import Data.STRef
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Array.MArray

swapElems :: Ix i => i -> i -> STArray s i e -> ST s ()
swapElems i j arr = do
  vi <- readArray arr i
  vj <- readArray arr j
  writeArray arr i vj
  writeArray arr j vi

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort xs = elems $ runSTArray $ do
  let n = length xs
  arr <- newListArray (0, n - 1) xs
  forM_ [0..n-1] $ \i ->
    forM_ [0..n-i-2] $ \j -> do
      x1 <- readArray arr j
      x2 <- readArray arr (j+1)
      when (x1 > x2) (swapElems j (j+1) arr)
  return arr

selectionSort :: (Ord a) => [a] -> [a]
selectionSort xs = elems $ runSTArray $ do
  let n = length xs
  arr <- newListArray (0, n - 1) xs
  forM_ [0..n-2] $ \i ->
    forM_ [i+1..n-1] $ \j -> do
      x1 <- readArray arr i
      x2 <- readArray arr j
      when (x1 > x2) (swapElems i j arr)
  return arr

test = (bubbleSort [5,8,12,5,1,7,9,11] == [1,5,5,7,8,9,11,12])
     && (selectionSort [21,11,10,5,8,15,2,34] == [2,5,8,10,11,15,21,34])
