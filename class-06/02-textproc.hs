{-
  Разработайте утилиту со следующими возможностями:
  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}
import System.Environment
import System.IO
import System.Directory
import System.Random
import Data.List
import Data.Char

-- 1) Подсчёт количества строк в заданном текстовом файле
linesCount :: FilePath -> IO ()
linesCount fname = do
  contents <- readFile fname
  putStrLn $ show $ length $ lines contents   

-- 2) Добавление заданной строки в начало (конец) заданного файла.
-- Для добавления в начало flag == first, иначе добавление в конец
addStr :: String -> String -> FilePath -> IO ()
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
fileToUpper :: FilePath -> IO ()
fileToUpper fname = do
  contents <- readFile fname
  putStrLn $ map toUpper contents 

-- 4) Построчное слияние двух заданных файлов (каждая строка первого файла соединяется с соответствующей строкой второго файла)
filesMerge :: FilePath -> FilePath -> IO ()
filesMerge fname1 fname2 = do
  contents1 <- readFile fname1
  contents2 <- readFile fname2
  writeFile "merge.txt" $ unlines (zipWith (\x y -> x ++ " " ++ y) (lines contents1) $ lines contents2)

-- 5) Генерация случайного текстового файла (случайность должна ограничиваться максимальным количеством строк в файле и символов в строке)
{-randomFile :: Int -> Int -> IO ()
randomFile max_ln max_ch = do
  gen <- newStdGen
  let numb_ln = fst $ randomR (1, max_ln) gen :: Int
  let numb_ch = fst $ randomR (1, max_ch) gen :: Int
  let text = take numb_ln $ take numb_ch $ randomRs ('a','z') gen
  writeFile "random.txt" $ unlines text-}

-- Выполняет нужное действие в зависимости от первого параметра
action :: Int -> [String] -> IO ()
action 1 (x:_) = linesCount x
action 2 (x:y:z:_) = addStr x y z
action 3 (x:_) = fileToUpper x
action 4 (x:y:_) = filesMerge x y
action 5 xs = undefined

main = do
  (arg0 : args) <- getArgs
  action (read arg0) args
  
