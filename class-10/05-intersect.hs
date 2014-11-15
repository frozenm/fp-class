{- Пользуясь списком как монадой, вычислите пересечение  заданных списков -}
import Control.Monad

intersect :: Eq a => [[a]] -> [a]
intersect [] = []
intersect (x0 : xs)= foldr (\acc x -> return x >>= filter (`elem` acc)) x0 xs
