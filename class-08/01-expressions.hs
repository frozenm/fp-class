
{-
  Имеется тип данных для представления арифметических выражений с целыми числами
  и функция для вычисления его значения. 
-}

data Expr = I Int            -- целочисленная константа
            | Add Expr Expr   -- сумма двух выражений
            | Mul Expr Expr   -- произведение двух выражений

eval :: Expr -> Int
eval (I n) = n
eval (e1 `Add` e2) = eval e1 + eval e2
eval (e1 `Mul` e2) = eval e1 * eval e2

{-
  Реализуйте для этого типа экземпляр класса типов Eq так,
  чтобы равными считались любые два выражения, значения которых
  совпадают.
-}

instance Eq Expr where
  x == y = eval x == eval y

{-
  Реализуйте для этого типа экземпляр класса типов Show так,
  чтобы выполнялись традиционные требования к записи арифметических
  выражений (и имеющиеся тесты). Для упрощения задания можно считать,
  что все числа в выражении положительные.
-}

instance Show Expr where
  show (I n) = show n
  show (Add e1 e2) = show e1 ++ "+" ++ show e2
  show (Mul (Add a1 a2) (Add a3 a4)) = "(" ++ show (Add a1 a2) ++ ")*(" ++ show (Add a3 a4) ++ ")"
  show (Mul e1 (Add a3 a4)) = show e1 ++ "*(" ++ show (Add a3 a4) ++ ")"
  show (Mul (Add a1 a2) e2) = "(" ++ show (Add a1 a2) ++ ")*" ++ show e2
  show (Mul e1 e2) = show e1 ++ "*" ++ show e2

-- Тесты
test = all (== expr 4) exprs
       && all (/= (I 10)) exprs
       && map show exprs == ["(5+1)*7", "6+6*6", "12+17+13", "42"]
  where
    expr 1 = (I 5 `Add` I 1) `Mul` I 7
    expr 2 = I 6 `Add` (I 6`Mul` I 6)
    expr 3 = (I 12 `Add` I 17) `Add` I 13
    expr 4 = I 42

    exprs = map expr [1..4]

{-
  Напишите экземпляр класса типов Ord, который сравнивает выражения по их значению.
-}

instance Ord Expr where
  compare e1 e2 = compare (eval e1) (eval e2)

