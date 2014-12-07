module ParseNumbers where

import Parser
import SimpleParsers
import Control.Applicative hiding (many, optional)
import Control.Monad


addition' = do
  n <- digit
  char '+'
  m <- digit
  return $ n + m

addition = digit >>= rest
  where
    rest m = (liftM (+m) $ char '+' >> digit) <|> return m


natural = foldl1 (\m n -> m *10 + n) `liftM` many1 digit


integer :: Parser Int
integer = (*) <$> minus <*> natural
  where
    minus = (char '-' >> return (-1)) <|> return 1


intList = bracket "[" "]" $ sepBy (token integer) (symbol ",")

countNumb :: Int -> Float
countNumb 0 = 1.0
countNumb x = 10.0 * countNumb (x `div` 10)

float :: Parser Float
float = (*) <$> minus <*> (number <|> fromIntegral <$> natural)
  where
    number = do
      a <- natural
      char '.'
      b <- natural
      return $ fromIntegral a + (fromIntegral b / countNumb b)
    minus = (char '-' >> return (-1)) <|> return 1

complex :: Parser (Float, Float)
complex = bracket "(" ")" $ do
  x <- token float
  char ','
  y <- token float
  return $ (x, y)
