import System.Environment
import Control.Monad.Instances
import Data.Maybe
import System.Random

{-
  Напишите функцию reduce, принимающую один целочисленный аргумент a и возвращающую 0,
  если аргумент делится на 3, a^2, если он на 3 не делится и является при этом нечётным,
  a^3 в остальных случаях.
-}

reduce :: Integral a => a -> a
reduce a 
  | a `mod` 3 == 0 = 0
  | odd a = a^2
  | otherwise = a^3

{-
  Напишите функцию, применяющую функцию reduce заданное количество раз к значению в контексте,
  являющемся функтором:
-}

reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
reduceNF n fa = foldl (\acc _ -> fmap reduce acc) fa [1..n]

{-
  Реализуйте следующие функции-преобразователи произвольным, но, желательно, осмысленным и
  нетривиальным способом.
-}

toList :: Integral a => [(a, a)]  -> [a]
toList = foldr (\(x,y) acc -> (max x y) : acc) []

toMaybe :: Integral a => [(a, a)]  -> Maybe a
toMaybe [] = Nothing
toMaybe xs = Just (maximum $ toList xs)

toEither :: Integral a => [(a, a)]  -> Either String a
toEither [] = Left "Empty list"
toEither xs = Right (maximum $ toList xs)

-- воспользуйтесь в этой функции случайными числами
toIO :: (Random a, Integral a) => [(a, a)]  -> IO a
toIO [] = return 0
toIO xs = do
  gen <- newStdGen
  let x = fst $ randomR (1, 100) gen
  return $ max x (maximum $ toList xs)

{-
  В параметрах командной строки задано имя текстового файла, в каждой строке
  которого записана пара целых чисел, разделённых пробелами. Загрузите
  данные из файла в список пар целых чисел, преобразуйте этот список к
  значениям в контекстах [], Maybe, Either String и IO и примените к каждому
  из контекстов функцию reduceNF (значение N также должно браться из 
  параметров командной строки).
-}

parseArgs :: [String] -> (FilePath, Int)
parseArgs str = (head str, read $ last str)

readData :: FilePath -> IO [(Int, Int)]
readData fname = do
  content <- readFile fname
  return $ foldr (\x acc -> (parse x) : acc) [] $ lines content 
  where
    parse x = let l = words x in (read $ head l, read $ last l)

main = do
  (fname, n) <- parseArgs `fmap` getArgs
  ps <- readData fname
  print ps
  print $ reduceNF n (toList ps)
  print $ reduceNF n (toMaybe ps)
  print $ reduceNF n (toEither ps)
  reduceNF n (toIO ps) >>= print

{-
  Подготовьте несколько тестовых файлов, демонстрирующих особенности различных контекстов.
  Скопируйте сюда результаты вызова программы на этих файлах.

  test1.txt 1
  [(34,67),(897,123),(1,987),(3333,65),(234,45),(98,89),(33,33),(1086,2375),(74,2),(193,528),(345,90)]
  [4489,0,0,0,0,941192,0,5640625,405224,0,0]
  Just 0
  Right 0
  0

  test2.txt 2
  []
  []
  Nothing
  Left "Empty list"
  0

  test3.txt 1
  [(59872,234),(23,12),(87,45),(97,12),(1234,846),(937,582),(5873,21)]
  [214620547022848,529,0,9409,1879080904,877969,34492129]
  Just 214620547022848
  Right 214620547022848
  214620547022848

-}


  

