{-
  Следующие типы задают множество состояний, алфавит и функцию переходов
  недетерминированного конечного автомата (НКА).
-}
import Data.List

type Alphabet = [Char]
type State = Int
type States = [State]
type AcceptingStates = [State]
type InitialState = State
type TransitionFunction = State -> Char -> States
type NFA = (Alphabet, States, InitialState, TransitionFunction, AcceptingStates)

-- пример НКА
nfa_ex :: NFA
nfa_ex = (['0','1'], [1, 2], 1, tf, [2])
  where
    tf 1 '0' = [1]
    tf 1 '1' = [1, 2]

-- Напишите функцию, определяющую, корректно ли задан НКА
isCorrect :: NFA -> Bool
isCorrect (alph, st, is, tf, as) = (not $ null alph) && (not $ null st) && (is `elem` st) && (as `intersect` st == as)


-- Напишите функцию, определяющую, допускает ли НКА заданное слово
-- Будем считать, что из финальных состояний переходить никуда нельзя 
accept :: NFA -> String -> Bool
accept (alph, st, is, tf, as) str = strIsOk && strIsAccept
  where
   strIsOk = foldl (\acc x -> (x `elem` alph) && acc) True str
   strIsAccept = any (\x -> x `elem` as) $ foldl step [is] str
   step states ch = concat $ map (\x -> if x `elem` as then [] else tf x ch) states

-- Постройте ещё как минимум три примера НКА
-- Слова вида a+b+
nfa1 :: NFA
nfa1 = (['a','b'], [1, 2, 3], 1, tf, [3])
  where
    tf 1 'a' = [1, 2]
    tf 1 'b' = []
    tf 2 'a' = []
    tf 2 'b' = [2, 3]

-- Слова вида 10*
nfa2 :: NFA
nfa2 = (['0', '1'], [1, 2, 3], 1, tf, [3])
  where
    tf 1 '0' = []
    tf 1 '1' = [2, 3]
    tf 2 '0' = [2, 3]
    tf 2 '1' = []

-- Слова вида 01+0
nfa3 :: NFA
nfa3 = (['0', '1'], [1, 2, 3, 4], 1, tf, [4])
  where
    tf 1 '0' = [2]
    tf 1 '1' = []
    tf 2 '0' = []
    tf 2 '1' = [2, 3]
    tf 3 '0' = [4]
    tf 3 '1' = []

{-
  Распределите заданные строки в соответствии с распознающими
  их НКА (одна строка может попасть в несколько групп).
-}

classify :: [NFA] -> [String] -> [(NFA, [String])]
classify nfas strs = foldr (\x acc -> (x, acceptedStr x strs) : acc) [] nfas
  where
    acceptedStr nfa sl = foldr (\x acc -> if accept nfa x then x : acc else acc) [] sl

showNFA :: NFA -> String
showNFA (alph, st, is, tf, as) = (concat $ map show alph) ++ ", " ++ (show st) ++ ", " ++ (show is) ++ ", tf, " ++ (show as)

printResult :: [(NFA, [String])] -> IO ()
printResult xs = print $ map (\(nfa, str) -> (showNFA nfa, str)) xs 

main = printResult $ classify [nfa1, nfa2, nfa3] ["0000", "10000", "ababa", "aaabb", "01110", "abbb", "1", "010"]
