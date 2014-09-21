-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms sec = (h, m, s)
  where 
    h = sec `div` 3600
    m = (sec - h*3600) `div` 60
    s = sec - h*3600 - m*60

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = h*3600 + m*60 + s

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = hms2sec (h, m, s)

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

-- б) периметр и площадь треугольника по координатам вершин.
triangle :: Point -> Point -> Point -> (Double, Double)
triangle a b c = (p, s)
  where
    r1 = distance a b
    r2 = distance b c
    r3 = distance a c
    p = r1 + r2 + r3
    p2 = p/2
    s = sqrt (p2*(p2-r1)*(p2-r2)*(p2-r3))

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs)
  | x `mod` 2 == 0 = 1 + nEven xs
  | otherwise = nEven xs 

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems [] = []
doubleElems (x:xs) = x*2 : doubleElems xs

-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs)
  | x `mod` 2 /= 0 = x : fltOdd xs
  | otherwise = fltOdd xs

-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
delNeg :: (Ord a, Num a) => [a] -> [a]
delNeg [] = []
delNeg (x:xs)
  | x >= 0 = x : delNeg xs
  | otherwise = delNeg xs

-- б) увеличить элементы с чётными значениями в два раза;
doubleEven :: Integral a => [a] -> [a]
doubleEven [] = []
doubleEven (x:xs)
  | x `mod` 2 == 0 = x*2 : doubleEven xs
  | otherwise = x : doubleEven xs

-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).
shuffle :: Num a => [a] -> [a]
shuffle [] = []
shuffle [_] = []
shuffle (x:y:xs) = y : x : shuffle xs

-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = (x+y) : combine_plus xs ys 

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.
combine_pair :: Num a => [a] -> [a] -> [(a, a)]
combine_pair [] ys = []
combine_pair xs [] = []
combine_pair (x:xs) (y:ys) = (x, y) : combine_pair xs ys

-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
firstN :: Integer -> [Integer]
firstN 0 = []
firstN n 
  | n > 0 = n : firstN (n-1)
  | otherwise = error "n should be >= 0"

-- б) в порядке возрастания.
firstN2 :: Integer -> [Integer]
firstN2 0 = []
firstN2 n 
  | n > 0 = firstN2' 1 n
  | otherwise = error "n should be >= 0"
  where 
    firstN2' a n 
      | a == n = [n]
      | otherwise = a : firstN2' (a+1) n

-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.
insertElem :: Num a => a -> [a] -> [a]
insertElem e [x] = [x] 
insertElem e (x:xs) = x : e : insertElem e xs


-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).
divide :: Eq a => [a] -> ([a], [a])
divide (x:xs) = divide' ([x], xs)
  where
    divide' ((x:xs), (y:ys))
      | x == y = divide' ((x:xs) ++ [y], ys)
      | otherwise = ((x:xs), (y:ys))

--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):

-- а) Доступ к элементу по индексу
elemAt :: [a] -> Int -> a
elemAt [] _ = error "n should be < length"
elemAt (x:xs) n
  | n == 0 = x
  | otherwise = elemAt xs (n-1)

-- б) Проверка, есть ли такой элемент в списке
contains :: Eq a => [a] -> a -> Bool
contains [] elem = False
contains (x:xs) elem
  | x == elem = True
  | otherwise = contains xs elem

-- в) Первые n элементов списка
first' :: [a] -> Int -> [a]
first' [] _ = []
first' (x:xs) n
  | n < 0 = error "n should be >= 0"
  | n == 0 = []
  | otherwise = x : first' xs (n-1)

-- г) Создает список из n штук элементов a
repeat' :: a -> Int -> [a]
repeat' a n
  | n == 0 = []
  | n < 0 = error "n should be >= 0"
  | otherwise = a : repeat' a (n-1)

-- д) Конкатенация списков
concat' :: [a] -> [a] -> [a]
concat' [] ys = ys
concat' (x:xs) ys = x : concat' xs ys

-- е) Группирует одинаковые элементы списка
group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = filter' (==) x (x:xs) : (group (filter' (/=) x (x:xs)))
  where 
    filter' _ _ [] = [] 
    filter' f x (x2:xs) 
      | f x x2 = x2 : filter' f x xs
      | otherwise = filter' f x xs

-- ж) Пронумеровывает элементы списка
numbering :: [a] -> [(Int, a)]
numbering (x:xs) = number (x:xs) 0
  where
    number [] n = []
    number (x:xs) n = (n,x) : number xs (n+1)

-- з) Оставляет в списке только те элементы, которые совпадают с первым
like_first :: Eq a => [a] -> [a]
like_first (x:xs) = x : like x xs
  where
    like _ [] = []
    like x (x2:xs)
      | x2 == x = x2 : like x xs
      | otherwise = like x xs

