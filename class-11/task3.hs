{-
  Пользуясь монадой State, реализовать функции для работы с очередью: enqueue и dequeue.
-}

import Control.Monad.State

type Queue = [Int]

enqueue :: Int -> State Queue ()
enqueue x = do
  xs <- get
  put (xs ++ [x])

dequeue :: State Queue Int
dequeue = do
  (x:xs) <- get
  put xs
  return x

queueManip :: State Queue Int
queueManip = do
  enqueue 5
  enqueue 2
  a <- dequeue
  enqueue a
  dequeue

test = runState queueManip [1,2,3,4] == (2, [3,4,5,2,1])
  
