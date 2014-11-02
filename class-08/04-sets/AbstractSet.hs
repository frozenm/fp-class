module AbstractSet where

class AbstractSet a where
  empty ::  a
  isEmpty :: a -> Bool
  add :: a -> Int -> a
  contains :: a -> Int -> Bool
