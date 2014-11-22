{-
   Модифицируйте представленное на лекции решение задачи о запросе пароля,
   удовлетворяющего требованиям по стойкости, следующим образом:
   - в командной строке задаются ограничения (минимальная длина, наличие букв,
     наличие цифр, наличие знаков пунктуации);
   - к стеку монад добавляется монада Reader, и с её помощью организуется
     доступ к ограничениям в функции isValid;
   - все попытки ввода пароля протоколируются средствами монады Writer.
-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Char

getLength :: Reader String Int
getLength = do
  n <- ask
  return $ read n

getFlag :: Reader String Bool
getFlag = do
  n <- ask
  return (if n == "true" then True else False)

isValid :: String -> Int -> Bool -> Bool -> Bool -> Bool
isValid s len f1 f2 f3 = (length s >= len) 
                         && (if f1 then any isAlpha s else True) 
                         && (if f2 then any isNumber s else True) 
                         && (if f3 then any isPunctuation s else True)

getValidPassword :: MaybeT IO String
getValidPassword = undefined {-do
  lift $ putStrLn "Введите новый пароль:"
  s <- lift getLine
  guard (isValid s)
  return s-}
 
askPassword :: MaybeT IO ()
askPassword = do
  value <- msum $ repeat getValidPassword
  lift $ putStrLn "Сохранение в базе данных..."

main = runMaybeT askPassword
