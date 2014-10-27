{-
  Соберите следующую информацию по текстовому файлу kapitan.txt:
  Постарайтесь использовать в решении наиболее подходящие структуры данных, избегайте повторные
  вычисления и заведомо неэффективные операции. Проверьте написанную программу на трёх других
  текстах того же или большего размера. Сравните результаты.
-}

import Data.Char
import Data.List
import Data.Function (on)
import qualified Data.Map as Map
import System.Environment

-- Загружаем текст в map
textToMap :: (Ord a) => [a] -> Map.Map a Int 
textToMap = foldl (\acc x -> Map.insertWith (+) x 1 acc) Map.empty

-- 1) самый часто используемый знак препинания;
mostFreqPunct:: String -> (Char, Int)
mostFreqPunct text = maximumBy (compare `on` snd) $ Map.toList $ textToMap $ filter isPunctuation text
     
-- 2) 50 наиболее часто используемых в тексте слов;
mostFreqWords :: String -> [(String, Int)]
mostFreqWords text = take 50 $ sortBy (flip compare `on` snd) $ Map.toList $ textToMap $ filter (/= "") $ map (map toLower . filter (\x -> (isLetter x) || (x =='\''))) $ words text

-- 3) частоты символов
mostFreqChars :: String -> [(Char, Int)]
mostFreqChars text = take 5 $ sortBy (flip compare `on` snd) $ Map.toList $ textToMap $ filter isLetter text

main = do
  (fname : _) <- getArgs
  content <- readFile fname
  putStrLn ("Most frequent punctuation:" ++ (show $ mostFreqPunct content))
  putStrLn ("Most frequent words:" ++ (show $ mostFreqWords content))
  putStrLn ("Most frequent chars:" ++ (show $ mostFreqChars content))
