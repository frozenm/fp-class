{-
   Запрограммируйте игру «Быки и коровы» (https://ru.wikipedia.org/wiki/Быки_и_коровы)
   в варианте «компьютер загадывает — пользователь отгадывает».
-}

import System.Environment
import System.Random
import Data.List

type Bull = Int
type Cow = Int

-- Проверяет, все ли цифры различны
isDifferent :: Int -> Bool
isDifferent number = ch0 /= ch1 && ch0 /= ch2 && ch0 /= ch3 && ch1 /= ch2 && ch1 /= ch3 && ch2 /= ch3
  where
    str = show number
    ch0 = str !! 0
    ch1 = str !! 1
    ch2 = str !! 2
    ch3 = str !! 3

-- Задумываем число
secretNumber :: (RandomGen g) => g -> Int
secretNumber gen = head $ filter isDifferent $ randomRs (1000,9999) gen

rightAnswer :: String -> Stirng -> (Bull, Cow)
rightAnswer numb1 numb2 = undefined

main = do
  gen <- newStdGen
  putStrLn $ show $ secretNumber gen
