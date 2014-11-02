import AbstractQueue
import qualified Queue as Q
import qualified FastQueue as FQ
import qualified SeqQueue as SQ
import System.Random
import System.Environment

-- Список случайных целых чисел
randomList :: Int -> IO [Int]
randomList n = do
  gen <- newStdGen
  return $ take n $ randomRs(1, 1000) gen 

-- Добавляем в очередь элементы из списка (n штук) и извлекаем n-1 элемент
operation :: (AbstractQueue a) => a Int -> [Int] -> a Int
operation queue list = deqN (length list - 1) $ foldl (enqueue) queue list 
  where
    deqN 0 q = q
    deqN n q = deqN (n-1) $ snd $ dequeue q 

-- Добавляем и извлекаем элементы numb раз
process :: (AbstractQueue a) => a Int -> [Int] -> Int -> a Int
process queue list numb = process' queue list 1 numb
  where 
    process' q l i n
      | n == 1 = operation q l
      | i == 1 = process' (enqueue q $ head l) (tail l) 2 n 
      | i == n = operation q l
      | otherwise = process' (operation q l) (drop i l) (i + 1) n 

-- Преобразуем очередь к списку
queueToList :: (AbstractQueue a) => a Int -> [Int]
queueToList queue
  | isEmpty queue = []
  | otherwise = let (x, q) = dequeue queue in x : queueToList q

-- Оставшиеся элементы очереди после всех действий
remainQueue :: (AbstractQueue a) => a Int -> [Int] -> Int -> IO [Int]
remainQueue queue list n = return $ queueToList $ process queue list n

-- Нужная длина списка случайных чисел для заданного n
listCount 1 = 1
listCount n = n + listCount (n-1)

main = do
  (arg0 : _) <- getArgs
  n <- readIO arg0
  list <- randomList $ listCount n
  l1 <- remainQueue (empty :: Q.Queue Int) list n
  l2 <- remainQueue (empty :: FQ.Queue Int) list n
  l3 <- remainQueue (empty :: SQ.Queue Int) list n
  putStrLn $ show (l1 == l2 && l2 == l3 && length l1 == n)
