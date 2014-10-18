{-
  Дан текстовый файл с информацией о студентах факультета в следующем формате:

    ФАМИЛИЯ ИМЯ ОТЧЕСТВО;ВОЗРАСТ;КУРС;ГРУППА

  Имя этого файла задаётся параметром командной строки. Остальные параметры определяют,
  какое действие следует выполнить:

  3) Создать файлы (с именами "<КУРС>_<ГРУППА>.txt") со списками всех студентов групп в формате
        ФАМИЛИЯ И.О.

-}
import System.Environment
import System.IO
import Data.List

-- Разбивает строку по ;
studentInfo :: String -> [String]
studentInfo str = filter (/= ";") $ groupBy (\x y -> x /= ';' && y /= ';') str

-- Список студентов заданной группы
showOneGroup :: Int -> Int -> [[String]] -> [[String]]
showOneGroup course group students = filter (\[_,_,c,g] -> (read c == course) && (read g == group)) students  

-- 1) Вычислить средний возраст студентов заданной группы заданного курса
avgAge :: FilePath -> Int -> Int -> IO ()
avgAge fname course group = do
  contents <- readFile fname
  let students = showOneGroup course group $ map studentInfo $ lines contents
  let (avg, count) = foldl (\(av, c) [_,a,_,_] -> (av + read a, c + 1)) (0,0) students
  putStrLn $ show (avg / count)

-- Сортировка по курсу и группе
sortByGroups :: (Int, Int) -> (Int, Int) -> Ordering
sortByGroups (x,y) (z,t)
  | x < z = LT
  | x > z = GT
  | y < t = LT
  | y > t = GT
  | otherwise = EQ

-- 2) Вычислить количество студентов в каждой группе каждого курса
countStudents :: FilePath -> IO ()
countStudents fname = do
  contents <- readFile fname
  let list = group $ sortBy sortByGroups $ map (\[_,_,c,g] -> (read c, read g)) $ map studentInfo $ lines contents 
  let countedlist = map (\xs -> (head xs, length xs)) list
  putStrLn $ unlines $ map (\(x,y) -> show x ++ ": " ++ show y) countedlist 

-- Выполняет нужное действие в зависимости от первого параметра
action :: Int -> [String] -> IO ()
action 1 (f:c:g:_) = avgAge f (read c) (read g)
action 2 (x:_) = countStudents x
action 3 (x:_) = undefined

main = do
  (arg0 : args) <- getArgs
  action (read arg0) args
