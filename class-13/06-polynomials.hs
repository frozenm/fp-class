{-# LANGUAGE EmptyDataDecls #-}

import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative hiding (many, optional)
import Control.Monad

{-
   Определите тип для многочлена с вещественными коэффициентами.
-}

data Poly = Mono Float Int | Bin Op Poly Poly
  deriving Show

data Op = Plus | Minus
  deriving Show 

{-
  Реализуйте парсер для многочленов (примеры в файле poly.txt).
-}

poly :: Parser Poly
poly = token (mono >>= rest addop mono)
  where
    rest op unit e1 = optional e1 $ do 
      p <- op
      e2 <- unit
      rest op unit $ Bin p e1 e2
    addop = binop ("+", Plus) ("-", Minus)
    binop (s1, cons1) (s2, cons2) =
          (symbol s1 >> return cons1) <|>
          (symbol s2 >> return cons2)
    mono = token $ do
      c <- optional 1 float
      p <- optional 0 ((string "x^" >> natural) <|> (string "x" >> return 1))
      return $ Mono c p

{-
  Напишите функцию, которая вычисляет частное и остаток при делении многочлена на   многочлен.
-}

divmod :: Poly -> Poly -> (Poly, Poly)
divmod = undefined

{-
  Напишите функцию, которая вычисляет наибольший общий делитель двух многочленов.
-}

poly_gcd :: Poly -> Poly -> Poly
poly_gcd = undefined

{-
  Напишите функцию, которая вычисляет наибольший общий делитель списка многочленов.
  Не забудьте воспользоваться свёрткой.
-}

poly_gcd_list :: [Poly] -> Poly
poly_gcd_list = undefined

{-
  Дан текстовый файл, в каждой строке которого записан один многочлен. Вычислите   наибольший
  общий делитель многочленов из файла. Предусмотрите вывод соответствующего сообщения,
если какая-либо строка файла имеет некорректный формат.
-}

poly_gcd_file :: FilePath -> IO (Either String Poly)
poly_gcd_file = undefined

{-
  В параметрах командной строки задано имя файла с многочленами. Найти их наибольший   общий делитель.
  Предусмотреть корректную обработку ошибок (неправильное количество параметров командной строки,
  отсутствие файла, неверный формат файла и пр.).
-}
main = undefined
