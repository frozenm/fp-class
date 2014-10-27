{-
   Напишите программу обработки квадратных матриц (на массивах) со следующими возможностями:

  Задание на обработку может выглядеть, к примеру, следующим образом (здесь вычисляется матричное
  выражение (A + B) * C):

    LOAD a FROM matr1.txt
    LOAD b FROM matr2.txt
    LOAD c FROM matr3.txt
    CALC d AS PLUS a b
    CALC e AS MULT d c
    SAVE d TO matr4.txt

   Параметром командной строки должно быть имя файла со сценарием указанного или подобного ему вида.
-}
import System.Environment
import Data.Array.IArray
import Data.List

-- 1) чтение матрицы из тестового файла;
readMatrix :: FilePath -> IO (Array (Int, Int) Int)
readMatrix fname = do
  content <- readFile fname
  let matrixStr = lines content 
  let n = length matrixStr
  return $ listArray ((1, 1), (n, n)) $ (map read $ concat $ map words matrixStr)

-- 2) запись матрицы в текстовый файл;
writeMatrix :: Array (Int, Int) Int -> FilePath -> IO ()
writeMatrix matrix fname = do
  writeFile fname $ unlines $ map (tail . foldl (\acc x -> acc ++ " " ++ x) "") $ splitN (map show $ elems matrix) (fst $ snd $ bounds matrix)
  where
    splitN [] _ = []
    splitN xs n = [take n xs] ++ (splitN (drop n xs) n)

-- 3) сумма матриц;
sumMatrix :: Array (Int, Int) Int -> Array (Int, Int) Int -> IO (Array (Int, Int) Int)
sumMatrix x y = do
  return $ array resultBounds [((r,c), x!(r,c) + y!(r,c)) | r <- range(lr, ur), c <- range(lc, uc)]
  where
    bx@((lr, lc), (ur, uc)) = bounds x
    resultBounds
      | bx == bounds y = bx
      | otherwise = error "sumMatrix: incompatible bounds"

main = do
  m1 <- readMatrix "matrix.txt"
  m2 <- readMatrix "matrix.txt"
  sumM <- sumMatrix m1 m2
  writeMatrix sumM "sum.txt"
