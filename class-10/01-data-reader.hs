{-
   Дан текстовый файл, содержащий данные о нескольких студентах в следующем формате: три последовательные
   строки соответствуют имени, возрасту и номеру группы (например, 4.8). Определить соответствующий тип
   данных (data) и организовать чтение файла в список значений этого типа.

   Для двух данных файлов объединить загруженные списки в один список, упорядоченный по имени и сохранить
   результат в новый файл того же формата. Указание: всюду следует использовать монадический синтаксис
   (операции >>= и >>, функции ap, liftM и другие функции монадической обработки данных, использование
   блока do не допускается).
-}
import Control.Monad
import Data.List
import Data.Ord

data Student = Student {
  name :: String
  , age :: Int
  , group :: (Int, Int) }

instance Show Student where
  show (Student n a (c, g)) = n ++ "\n" ++ (show a) ++ "\n" ++ (show c) ++ "." ++ (show g)

-- Группирует список по 3 элемента
groupBy3 :: [a] -> [[a]]
groupBy3 [] = []
groupBy3 xs = [take 3 xs] ++ (groupBy3 $ drop 3 xs)

-- Чтение файла в список студентов
loadFile :: FilePath -> IO [Student]
loadFile fname = readFile fname >>= (return . (foldr (\x acc -> parse x : acc) []) . groupBy3 . lines)
  where
    parse [n, a, c] = Student n (read a) (read (take 1 c), read (drop 2 c))

-- Запись в файл
writeToFile :: FilePath -> [Student] -> IO ()
writeToFile fname list = writeFile fname $ unlines $ map show list

main = (++) `liftM` loadFile "students1.txt" `ap` loadFile "students2.txt" >>= writeToFile "students.txt" . sortBy (comparing name)
