{-
   Запрограммируйте игру «Быки и коровы» (https://ru.wikipedia.org/wiki/Быки_и_коровы)
   в варианте «компьютер загадывает — пользователь отгадывает».
-}

import System.Environment
import System.Random
import Data.List
import Control.Monad (when)

-- Задумываем число
secretNumber :: (RandomGen g) => g -> Int
secretNumber gen = head $ filter isDifferent $ randomRs (1000,9999) gen
  where
    isDifferent number = nub (show number) == show number

-- Считаем быков и коров
check :: String -> String -> (Int, Int)
check numb1 numb2 = (bulls, cows)
  where
    bulls = length $ filter (\(x,y) -> x == y) $ zip numb1 numb2
    cows = length (intersect numb1 numb2) - bulls 

-- Играемся
game :: Int -> IO ()
game secret_number = do
  numberString <- getLine
  when (not $ null numberString) $ do
  let number = read numberString 
  let (bulls, cows) = check (show secret_number) numberString 
  if secret_number == number
    then putStrLn "You win!"
    else do
      putStrLn $ "Wrong! You have " ++ show bulls ++ " bulls and " ++ show cows ++ " cows"
      game secret_number

main = do
  gen <- newStdGen
  let secret_number = secretNumber gen
  putStrLn "I have a secret 4-digit number. What is it?"
  game secret_number
