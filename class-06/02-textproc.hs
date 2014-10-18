{-
  Разработайте утилиту со следующими возможностями:
  4) построчное слияние двух заданных файлов (каждая строка первого файла
     соединяется с соответствующей строкой второго файла);
  5) генерация случайного текстового файла (случайность должна ограничиваться
     максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}
import System.Environment
import System.IO
import System.Directory
import Data.List
import Data.Char

-- 1) Подсчёт количества строк в заданном текстовом файле
linesCount :: FilePath -> IO()
linesCount fname = do
  contents <- readFile fname
  putStrLn $ show $ length $ lines contents   

-- 2) Добавление заданной строки в начало (конец) заданного файла.
-- Для добавления в начало flag == first, иначе добавление в конец
addStr :: String -> String -> FilePath -> IO()
addStr flag s fname = do
  contents <- readFile fname
  if (flag == "first") then do
    writeFile "tmp.txt" (s ++ "\n" ++ contents)
  else do
    writeFile "tmp.txt" (contents ++ s ++ "\n")
  tmpContents <- readFile "tmp.txt"
  writeFile fname tmpContents
  removeFile "tmp.txt"

-- 3) Преобразование всех буквенных символов заданного файла к верхнему регистру (результат выводится на консоль)
fileToUpper :: FilePath -> IO()
fileToUpper fname = do
  contents <- readFile fname
  putStrLn $ map toUpper contents 

-- Выполняет нужное действие в зависимости от первого параметра
action :: Int -> [String] -> IO()
action 1 (x:_) = linesCount x
action 2 (x:y:z:_) = addStr x y z
action 3 (x:_) = fileToUpper x
action 4 xs = undefined
action 5 xs = undefined

main = do
  (arg0 : args) <- getArgs
  action (read arg0) args
