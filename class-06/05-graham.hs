{-
   Дописать к реализованному ранее алгоритму Грэхема основную программу, которая принимает
   на вход файл со списком точек и создаёт файл со списком точек, образующих выпуклую оболочку.

   Для отыскания пути к импортируемому модулю следует использовать параметр -i командной строки
   (для ghc и ghci), например:

     $ ghc 05-graham.hs  -o graham -i../class-05/3-Graham_scan/
-}

import GrahamScan
import System.Environment

convertToPoints :: [(Double, Double)] -> [Point]
convertToPoints = map (\(x,y) -> (Point x y)) 

pointToString :: [Point] -> [String]
pointToString = map (\(Point x y) -> "(" ++ show x ++ " ; " ++ show y ++ ")")

findConvexHull :: FilePath -> FilePath -> IO ()
findConvexHull fname1 fname2 = do
  contents <- readFile fname1
  let hull = graham_scan $ convertToPoints $ map (\x -> read x :: (Double, Double)) $ lines contents
  writeFile fname2 $ unlines $ pointToString hull

main = do
  (arg1:arg2:_) <- getArgs
  findConvexHull arg1 arg2
