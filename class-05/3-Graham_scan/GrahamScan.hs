{-# LANGUAGE EmptyDataDecls #-}

module GrahamScan where

import Data.List
import Data.Function

-- 1. Определить тип Point для хранения информации о точке на вещественной плоскости.

data Point = Point {
  x :: Double
  ,y :: Double} deriving (Show, Eq)
  
{-
  2. Если заданы три точки a, b, c, можно рассматривать направление поворота от отрезка прямой,
  заключённого между точками a и b, к отрезку прямой, заключённому между точками b и c. Поворот
  может осуществляться влево, вправо или отрезки могут лежать на одной прямой — для представления
  этих трёх возможностей определить специальный тип Direction.
-}

data Direction = Left_Rotate | Right_Rotate | Straight
  deriving (Show, Eq)

{-
  3. Определить функцию, которая принимает список точек и вычисляет список направлений поворотов
  для каждых трёх последовательных точек. Например, для списка точек [a, b, c, d, e] она возвращает
  список поворотов для точек [a, b, c], [b, c, d] и [c, d, e]. При написании этой функции рекомендуется
  определить несколько вспомогательных функций.
-}

turn :: Point -> Point -> Point -> Direction
turn a b c = if (res < 0) then Left_Rotate else (if (res > 0) then Right_Rotate else Straight)
  where
    res = (x c - x a)*(y b - y a) - (y c - y a)*(x b - x a)

directions :: [Point] -> [Direction]
directions [] = []
directions [_] = []
directions [_,_] = []
directions (x0:x1:xs) = snd $ foldl (\((a,b),acc) c -> ((b,c), acc ++ [turn a b c])) ((x0,x1),[]) xs

{-
  4. Пользуясь решениями предыдущих упражнений, реализовать алгоритм Грэхема нахождения выпуклой
  оболочки множества точек на вещественной плоскости. Описание алгоритма можно взять в английском
  (Graham scan) или русском разделах Википедии. Там же можно разобраться с тем, что именно называют
  выпуклой оболочкой (convex hull). Визуализация порядка работы алгоритма имеется на Youtube:
  http://www.youtube.com/watch?v=BTgjXwhoMuI
-}

-- Самая левая нижняя точка
firstPoint :: [Point] -> Point
firstPoint [p] = p
firstPoint (p:ps) = minY p (firstPoint ps) 
  where
    minY a b
        | y a > y b = b
        | y a < y b = a
        | x a < x b = a
        | otherwise = b

-- Сортировка точек по углу
sortAngle :: Point -> [Point] -> [Point]
sortAngle x0 xs = tail (sortBy (compare `on` compkey x0) xs) 
  where
    compkey a b = (atan2 (y b - y a) (x b - x a), abs (x b - x a))

graham_scan :: [Point] -> [Point]
graham_scan [] = []
graham_scan [x] = [x]
graham_scan [x0, x1] = [x0, x1]
graham_scan points = foldl (\(x2:x1:xs) x3 -> if (turn x1 x2 x3 == Right_Rotate) then (x3:x1:xs) else (x3:x2:x1:xs)) (head sortedList : [p]) (tail sortedList) 
  where
    sortedList = sortAngle p points
    p = firstPoint points

{-
  5. Приведите несколько примеров работы функции graham_scan.
-}

graham_scan_test1 = graham_scan [(Point 2 5), (Point 5 2), (Point 1 2), (Point 4 4), (Point 6 5)] == [(Point 2 5), (Point 6 5), (Point 5 2), (Point 1 2)]

graham_scan_test2 = graham_scan [(Point 2 4), (Point 6 2), (Point 4 6), (Point 1 1), (Point 4 3), (Point 5 5), (Point 2 5)] == [(Point 2 5), (Point 4 6), (Point 5 5), (Point 6 2), (Point 1 1)]
