{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и вывести различные числа, встречающиеся
  в каждом из заданных текстовых файлов. Указание: в решении следует воспользоваться множествами.
-}

import System.Environment
import qualified Data.IntSet as Set

readNumFile :: FilePath -> IO [Int]
readNumFile fname = do
  content <- readFile fname
  return $ map read $ concatMap words $ lines content

solve :: [[Int]] -> (Int, [Int])
solve l = (Set.size set, Set.toList set)
  where
    set = foldl1 Set.intersection $ map Set.fromList l


main = getArgs >>= mapM readNumFile >>= print.solve

