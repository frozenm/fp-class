import Control.Monad 
import Control.Applicative
{-
  Модифицируйте имеющуюся реализацию задачи о канатоходце (лекция 9) следующим образом:
  1) реализуйте загрузку входных данных из файла следующего вида:
       R 2
       L 3
       R -1
       B
       L 1
     и вычисление соответствующего им результата (в решении может пригодиться 
     функция foldr (<=<) return — проверьте её тип для получения подсказки);
  2) замените монаду Maybe на Either String так, чтобы в случае падения канатоходца
     можно было получить информацию о его причинах (нарушение баланса и в какую
     сторону или банан на канате);
  3) реализуйте операцию landBoth, поддерживающую одновременное (атомарное) приземление
     птиц на оба конца шеста, и внесите соответствующие изменения в другие функции;
  5) реализуйте операцию unlandAll (одновременный вылет всех птиц с шеста) и внесите
     соответствующие изменения в другие функции;
  4) организуйте масштабное тестирование.
-}

type Birds = Int

type Pole = (Birds, Birds)

balance = 3

updatePole :: Pole -> Either String Pole
updatePole p = if unbalancedL p then Left "Unbalanced to left" else if unbalancedR p then Left "Unbalanced to right" else Right p
  where
    unbalancedL (l, r) = l - r > balance
    unbalancedR (l, r) = r - l > balance 

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right) = updatePole (left + n, right)

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right) = updatePole (left, right + n)

landBoth :: Birds -> Pole -> Either String Pole
landBoth n (left, right) = Right (left + n, right + n)

unlandAll :: Pole -> Either String Pole
unlandAll = const (Right (0, 0))

banana :: Pole -> Either String Pole
banana = const (Left "Banana on the pole")
 

--Вычисляет результат по строке вида "R 1\nL 3\n ...", T - landBoth, U - unlandAll
countResult :: String -> Either String Pole
countResult str = foldr (<=<) return actionList (0, 0)
  where
    actionList = reverse $ map parse $ lines str
    parse "B" = banana
    parse "U" = unlandAll
    parse str = let (a, n) = (head str, read $ drop 2 str :: Birds) in 
                if a == 'R' then landRight n else 
                if a == 'T' then landBoth n else landLeft n

tests = all test [1..4]
  where
    test 1 = (return (0, 0) >>= landRight 2 >>= landLeft 1 >>= landRight 3) == Left "Unbalanced to right"
    test 2 = (return (0, 0) >>= landRight 2 >>= landBoth 2 >>= landLeft 1) == Right (3, 4)
    test 3 = (return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1) == Left "Banana on the pole"
    test 4 = (return (0, 0) >>= landLeft 1 >>= unlandAll >>= landBoth 2 ) == Right (2, 2)
    --test 5 = (==) <$> ((readFile "pole.txt") >>= return . countResult) <*> pure (Right (2, 2))
