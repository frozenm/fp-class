{-
   Тип Parser может быть определён следуюшим образом:
-}

import Control.Monad

newtype Parser a = Parser { apply :: String -> Maybe (a, String) }

{-
   Определите экземпляры классов Monad и MonadPlus для типа Parser в этом случае:
-}

instance Monad Parser where
  return x = Parser (\s -> Just (x, s))
  p >>= q = Parser func
    where
      func s = let ps = apply p s in case ps of
                 Just (x, s') -> apply (q x) s'
                 Nothing -> Nothing
  fail _ = Parser (\s -> Nothing)

instance MonadPlus Parser where
  mzero = Parser (\s -> Nothing)
  p `mplus` q = Parser func
    where
      func s = let ps = apply p s in case ps of
                 Just x -> ps
                 Nothing -> apply q s
