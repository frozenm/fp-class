{-
  Написать программу, которая в зависимости от параметров командной строки
-}
import System.Environment
import System.IO
import System.Random
import Data.List

-- а) Генерирует случайный текстовый файл содержащий декартовы координаты точек на плоскости
generate :: FilePath -> IO ()
generate fname = do
  gen1 <- newStdGen
  gen2 <- newStdGen
  let numb = fst $ randomR (1, 20) gen1 :: Int
  writeFile fname (unlines $ map show $ zipWith (\x y -> (x,y)) (points numb gen1) (points numb gen2))
    where
      points n gen = take n $ randomRs (-100,100) gen :: [Int]  

-- б) Определяет по заданному файлу в указанном ранее формате количество точек в каждой из четвертей. Результат - список, где n-ый элемент = кол-во точек в n-ой четверти
countQuarters :: FilePath -> IO ()
countQuarters fname = do
  contents <- readFile fname
  putStrLn $ show $ foldl count [0,0,0,0] $ map (\x -> quarterNumber $ read x) $ lines contents
    where
      quarterNumber (x,y)
        | (x > 0) && (y > 0) = 1
        | (x < 0) && (y > 0) = 2
        | (x < 0) && (y < 0) = 3
        | (x > 0) && (y < 0) = 4
        | otherwise = 0
      count [acc1, acc2, acc3, acc4] x
        | x == 1 = [acc1+1, acc2, acc3, acc4]
        | x == 2 = [acc1, acc2+1, acc3, acc4]
        | x == 3 = [acc1, acc2, acc3+1, acc4]
        | x == 4 = [acc1, acc2, acc3, acc4+1]
        | otherwise = [acc1, acc2, acc3, acc4]

-- в) Отыскивает наиболее удалённую от начала координат точку
mostFurther :: FilePath -> IO ()
mostFurther fname = do
  contents <- readFile fname
  let points = map (\x -> read x) $ lines contents
  putStrLn $ show $ fst $ foldl (\(pt, max) x -> if (dist x > max) then (x, dist x) else (pt, max)) ((0,0),0) points 
  where
    dist (x,y) = sqrt (x^2 + y^2)

-- Выполняет нужное действие в зависимости от первого параметра
action :: Int -> [String] -> IO ()
action 1 (x:_) = generate x
action 2 (x:_) = countQuarters x
action 3 (x:_) = mostFurther x

main = do
  (arg0 : args) <- getArgs
  action (read arg0) args
