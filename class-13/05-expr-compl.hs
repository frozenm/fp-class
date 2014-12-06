import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative hiding (many, optional)
import Control.Monad

{-
   Добавьте поддержку вещественных и комплексных чисел из второго упражнения.
   Можете считать, что все числа в выражении являются комплексными (и можете
   не считать, если в состоянии красиво обойтись с типами и всё корректно
   проанализировать).
-}

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

data Expr = Con Int | Bin Op Expr Expr
  deriving Show
data Op = Plus | Minus | Mul | Div
  deriving Show

{-
expr   ::= term {addop term}*
term   ::= factor {mulop factor}*
factor ::= nat | '(' expr ')'
addop  ::= '+' | '-'
mulop  ::= '*' | '/'
-}

expr = token (term >>= rest addop term)
  where
    rest op unit e1 = optional e1 $ do 
        p <- op
        e2 <- unit
        rest op unit $ Bin p e1 e2
    term = token (factor >>= rest mulop factor)
    factor = token (constant <|> bracket "(" ")" expr)
    addop = binop ("+", Plus) ("-", Minus)
    mulop = binop ("*", Mul) ("/", Div)
    binop (s1, cons1) (s2, cons2) =
          (symbol s1 >> return cons1) <|>
          (symbol s2 >> return cons2)
    constant = Con `liftM` natural


