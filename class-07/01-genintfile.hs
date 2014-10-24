{-
  Создать текстовый файл, содержащий случайные целые числа, разделённые пробелами
  и символами перевода строки. В командной строке должны задаваться следующие параметры:
  1) имя создаваемого файла;
  2) диапазон генерируемых случайных чисел: от и до;
  3) количество чисел в строке;
  4) количество строк в файле.
-}

import System.Environment
import System.IO
import System.Directory
import System.Random

-- Формируем содержимое файла
generateText :: Int -> Int -> Int -> Int -> IO [String]
generateText from to numbers linesCount = do
  gen <- newStdGen
  sequence $ take linesCount $ repeat generateLine
  where
    generateLine = do
      gen <- newStdGen
      return . tail . foldl (\acc x -> acc ++ " " ++ x) "" $ map show $ take numbers (randomRs (from, to) gen :: [Int])

main = do
  (name : from : to : numbers : linesCount : args) <- getArgs
  contents <- generateText (read from) (read to) (read numbers) (read linesCount)
  writeFile name $ unlines contents
