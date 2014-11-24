{-
  Организовать вычисление значений функций sin и cos, пользуясь рядами Тейлора и     
  сохраняя каждое слагаемое в журнал посредством монады Writer. В тексте программы
  допускается только один вызов функции tell.
-}

import Control.Monad.Writer

eps = 1e-10

taylor :: Double -> Double -> Double -> Double -> Writer [Double] Double
taylor prevValue prevSummand n x = tell [prevSummand] >> 
                     if abs (prevSummand - nextSummand) < eps then return prevValue 
                     else taylor (prevValue + nextSummand) nextSummand (n + 2) x 
  where
    nextSummand = ((-1) * prevSummand * x * x) / (n+1) / (n+2)

sin' :: Double -> (Double, [Double])
sin' x = runWriter $ taylor x x 1 x

cos' :: Double -> (Double, [Double])
cos' x = runWriter $ taylor 1 1 0 x

