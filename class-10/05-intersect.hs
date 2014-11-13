{- Пользуясь списком как монадой, вычислите пересечение  заданных списков -}
import Data.List
import Control.Monad

intersect' :: Eq a => [[a]] -> [a]
intersect' = foldr undefined []
