import AbstractSet
import qualified ListSet as LS
import qualified TreeSet as TS
import System.Random

-- Список случайных целых чисел
randomList :: Int -> IO [Int]
randomList n = do
  gen <- newStdGen
  return $ take n $ randomRs(1, 1000) gen 

-- Добавляем во множество элементы из списка
operation :: (AbstractSet a) => a -> [Int] -> a
operation set list = foldl (add) set list 

-- Первая проверка
check1 :: (AbstractSet a) => a -> Bool
check1 s = contains (add (add (add s 3) 7) 6) 7  

-- Вторая проверка
check2 :: (AbstractSet a) => a -> [Int] -> Bool
check2 s list = contains set (head list) && contains set (head $ tail list)
  where
    set = operation s list

-- Третья проверка
check3 :: (AbstractSet a) => a -> Bool
check3 s = not $ contains (remove (add (add (add s 2) 2) 8) 2) 2 

main = do
  print $ check1 (empty :: LS.Set) && check1 (empty :: TS.Set)
  list <- randomList 15
  print $ check2 (empty :: LS.Set) list && check2 (empty :: TS.Set) list
  print $ check3 (empty :: LS.Set) && check3 (empty :: TS.Set)
