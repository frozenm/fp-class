{-# LANGUAGE EmptyDataDecls #-}

module GrahamScan where

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

graham_scan :: [Point] -> [Point]
graham_scan = undefined

{-
  5. Приведите несколько примеров работы функции graham_scan.
-}
