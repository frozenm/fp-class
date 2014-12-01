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
import System.Environment

isValid :: String -> [Int] -> Bool
isValid s (len : f1 : f2 : f3 : _) = (length s >= len) 
                         && (if f1 == 1 then any isAlpha s else True) 
                         && (if f2 == 1 then any isNumber s else True) 
                         && (if f3 == 1 then any isPunctuation s else True)

getValidPassword :: MaybeT (ReaderT [Int] (WriterT [String] IO)) String
getValidPassword = do
  xs <- ask
  liftIO $ putStrLn "Введите новый пароль:"
  s <- liftIO getLine
  tell [s]
  guard (isValid s xs)
  return s

askPassword :: MaybeT (ReaderT [Int] (WriterT [String] IO)) ()
askPassword = do
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Сохранение в базе данных..."

-- Аргументами командой строки являются целые числа, где 1 - включение ограничения, 0 - выключение.
main = do
  args <- getArgs
  (_, xs) <- runWriterT (runReaderT (runMaybeT askPassword) (map read args))
  print xs
