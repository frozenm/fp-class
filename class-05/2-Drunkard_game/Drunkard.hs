{-# LANGUAGE EmptyDataDecls #-}

module Drunkard where

{-
  1. Определить типы данных, необходимые для представления игральной карты в игре «Пьяница»,
  учитывая, что всего в колоде 52 карты.
-}

data Suit = Spades | Clubs | Diamonds | Hearts
  deriving (Show, Eq)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord)

data Card = Card Value Suit
  deriving (Show, Eq)

value :: Card -> Value
value (Card v _) = v

suit :: Card -> Suit
suit (Card _ s) = s

-- 2. Определить функцию, проверяющую, что две переданные ей карты одной масти.

sameSuit :: Card -> Card -> Bool
sameSuit c1 c2 = suit c1 == suit c2 

{-
  3. Определить функцию, проверяющую, что переданная ей первой карта старше второй
  (масть в игре «Пьяница» игнорируется). Возвращённое значение EQ означает, что обе
  карты одинакового старшинства.
-}

beats :: Card -> Card -> Ordering
c1 `beats` c2 = compare (value c1) (value c2)

{-
  4. Определить функцию, которая по паре списков карт возвращает новую пару списков карт
  с учетом правил игры «Пьяница» (один раунд игры): 
    * из вершин списков берутся две карты и добавляются в конец того списка, карта из
      которого старше оставшейся;
    * если первые взятые карты совпадают по достоинству, то из списков берутся и
      сравниваются следующие две карты (и так до тех пор, пока не будет определён победитель
      раунда).
-}

game_round :: ([Card], [Card]) -> ([Card], [Card])
game_round (xs, ys) = game_round' [] xs ys 
  where
    game_round' prev (x:xs) (y:ys)
      | x `beats` y == GT = (xs ++ prev ++ [x,y], ys)
      | x `beats` y == LT = (xs, ys ++ prev ++ [x,y])
      | otherwise = game_round' [x,y] xs ys

{-
  5. Определить функцию, которая по паре списков возвращает количество раундов, необходимых
  для завершения игры (одна из колод оказывается пустой), и номер победителя.
-}

data Winner = First | Second
  deriving (Show, Eq)

game :: ([Card], [Card]) -> (Winner, Int)
game (xs, ys) = game' 0 (xs, ys)
  where
    game' n ([], _) = (Second, n)
    game' n (_, []) = (First, n)
    game' n (c1, c2) = game' (n+1) $ game_round (c1, c2)

{-
  6. Приведите здесь результаты как минимум пяти запусков функции game (в каждом списке
  изначально должно быть не менее 10 карт).
-}

game_test1 = game ([(Card Two Spades),(Card Three Spades),(Card Three Clubs),(Card Jack Clubs),(Card Two Clubs),(Card Six Diamonds),(Card Ace Hearts),(Card Ten Clubs),(Card Seven Spades),(Card Eight Spades)], [(Card Two Diamonds),(Card Three Hearts),(Card Ace Diamonds),(Card King Hearts),(Card Two Hearts),(Card Queen Clubs),(Card Ace Spades),(Card Ten Diamonds),(Card Eight Clubs),(Card Nine Diamonds)]) == (Second, 5)

game_test2 = game ([(Card Two Spades),(Card Three Spades),(Card Four Spades),(Card Five Spades),(Card Six Spades),(Card Seven Spades),(Card Eight Spades),(Card Nine Spades),(Card Ten Spades),(Card Jack Spades)], [(Card Two Clubs),(Card Three Clubs),(Card Four Clubs),(Card Five Clubs),(Card Six Clubs),(Card Seven Clubs),(Card Eight Clubs),(Card Nine Clubs),(Card Ten Clubs),(Card Ten Diamonds)]) == (First, 1)

game_test3 = game ([(Card Two Diamonds),(Card Three Hearts),(Card Ace Diamonds),(Card King Hearts),(Card Two Hearts),(Card Queen Clubs),(Card Ace Spades),(Card Ten Diamonds),(Card Eight Clubs),(Card Nine Diamonds)], [(Card Two Spades),(Card Three Spades),(Card Three Clubs),(Card Jack Clubs),(Card Two Clubs),(Card Six Diamonds),(Card Ace Hearts),(Card Ten Clubs),(Card Seven Spades),(Card Eight Spades)]) == (First, 5)

game_test4 = game ([(Card Two Spades),(Card Three Spades),(Card Four Spades),(Card Five Spades),(Card Six Spades),(Card Seven Spades),(Card Eight Spades),(Card Nine Spades),(Card Ten Spades),(Card Jack Spades)], [(Card Three Clubs),(Card Four Clubs),(Card Five Clubs),(Card Six Clubs),(Card Seven Clubs),(Card Eight Clubs),(Card Nine Clubs),(Card Ten Clubs),(Card Ten Diamonds), (Card Queen Clubs)]) == (Second, 9)

game_test5 = game ([(Card Four Diamonds),(Card Four Spades),(Card Five Spades),(Card Six Spades),(Card Seven Spades),(Card Eight Spades),(Card Nine Spades),(Card Ten Spades),(Card Jack Spades), (Card Queen Spades)], [(Card Two Clubs),(Card Three Clubs),(Card Six Clubs),(Card Seven Clubs),(Card Eight Clubs),(Card Nine Clubs),(Card Ten Clubs),(Card Jack Diamonds),(Card Queen Clubs),(Card Ace Clubs)]) == (Second, 14)

{-
  7 (необязательное упражнение). Реализуйте версию функции game, которая помимо результатов
  игры возвращает запись всех ходов (карты, выкладываемые по ходу игры для сравнения).
-}

{-
  8 (необязательное упражнение). При выполнении функций из упражнений 4 и 5 возможно
  зацикливание. Чтобы его избежать, можно предусмотреть максимальное количество повторений
  (для раундов и ходов в рамках одного раунда). Подумайте, как обнаружить факт зацикливания
  в функции 4? Можно ли применить такой же подход в функции 5? Что нужно возвращать в случае
  обнаружения факта зацикливания? Измените соответствующим образом типовые аннотации и
  напишите безопасные по отношению к зацикливанию версии функций game_round и game.
-}
