import System.Environment
import Data.Monoid
import Data.Maybe

{-
  Некоторый датчик генерирует по пять сигналов в сутки, часть из которых
  не доходит до базовой станции. Полученные от датчика сведения представлены
  текстовым файлом, содержащим по одному целому числу в каждом строке. Если
  сигнал не был получен, вместо числа в файле записывается прочерк (символ '-').
-}

type SensorValue = Maybe Int
type SensorData = [SensorValue]

{- Напишите функцию, которая преобразует прочитанную из файла строку в список
   значений, полученных от датчика. -}

getData :: String -> SensorData
getData = map toMaybe . lines
  where
    toMaybe "-" = Nothing
    toMaybe x = Just (read x :: Int)

{- Напишите функцию, группирующую данные по суткам. -}

dataByDay :: SensorData -> [SensorData]
dataByDay [] = []
dataByDay xs = [take 5 xs] ++ (dataByDay $ drop 5 xs)

{-
  Посчитайте минимальное значение среди показаний датчика,
  полученных:
  а) первыми в течение суток;
  б) последними в течение суток.
  Если в некоторые сутки показания не были получены ни разу,
  такие сутки должны игнорироваться.

  Указание: в решении следует пользоваться возможностями моноидов First и Last,
  при этом должна быть написана одна функция, отвечающая на вопрос а) или б)
  в зависимости от значения логического параметра.
-}

minData1 :: Bool -> [SensorData] -> Int
minData1 needFirst sdata = minimum $ map (func needFirst) $ filter (any isJust) sdata
  where
    func flag xs = if flag then fromJust. getFirst . mconcat . map First $ xs else fromJust. getLast . mconcat . map Last $ xs

{-
  Посчитайте минимальное значение среди данных,
  полученных:
  а) как суммы всех показаний датчика за каждые сутки;
  б) как произведения всех показаний датчика за каждые сутки.
  Если в некоторые сутки показания не были получены ни разу,
  такие сутки должны игнорироваться.

  Указание: в решении следует пользоваться возможностями моноидов Sum, Product
  и Maybe a, где a — моноид, при этом должна быть написана одна функция, отвечающая
  на вопрос а) или б) в зависимости от значения логического параметра.
-}

minData2 :: Bool -> [SensorData] -> Int
minData2 needSum sdata = minimum $ map (func needSum) $ filter (any isJust) sdata
  where
    func flag xs = if flag then getSum . mconcat . map Sum $ map (fromMaybe 0) xs else getProduct . mconcat . map Product $ map (fromMaybe 1) xs

{- Попробуйте объединить две предыдущие функции в одну. -}

data SensorTask = NeedFirst | NeedLast | NeedSum | NeedProduct

minData :: SensorTask -> [SensorData] -> Int
minData st sdata = minimum $ map (func st) $ filter (any isJust) sdata
  where
    func NeedFirst xs = fromJust. getFirst . mconcat . map First $ xs
    func NeedLast xs = fromJust. getLast . mconcat . map Last $ xs
    func NeedSum xs = getSum . mconcat . map Sum $ map (fromMaybe 0) xs
    func NeedProduct xs = getProduct . mconcat . map Product $ map (fromMaybe 1) xs

{-
  Пользуясь моноидами All, Any и любыми другими, выясните следующую информацию:
  1) количество суток, за которые не было получено ни одного показания;
  2) количество суток, показания за которые получены полностью;
  3) количество суток, за которые было получено хотя бы одно показание;
  4) количество суток, сумма показаний за которые превосходит заданное число;
  5) количество суток, произведение показаний за которые превосходит заданное число;
  6) количество суток, первое показание за которые превосходит заданное число;
  7) количество суток, последнее показание за которые превосходит заданное число.

  Постарайтесь ответить на все вопросы, написав одну функцию.
-}

data SensorInfo = NoData | AllData | AnyData | SumData Int | ProductData Int | FirstData Int | LastData Int

getInfo :: SensorInfo -> [SensorData] -> Int
getInfo NoData sdata = length $ filter id $ map (getAll . mconcat. map All . map isNothing) sdata
getInfo AllData sdata = length $ filter id $ map (getAll . mconcat. map All . map isJust) sdata
getInfo AnyData sdata = length $ filter id $ map (getAny . mconcat. map Any . map isNothing) sdata
getInfo (SumData n) sdata = length $ filter (>n) $ map (getSum . mconcat . map Sum . map (fromMaybe 0)) $ filter (any isJust) sdata
getInfo (ProductData n) sdata = length $ filter (>n) $ map (getProduct . mconcat . map Product . map (fromMaybe 1)) $ filter (any isJust) sdata
getInfo (FirstData n) sdata = length $ filter (>n) $ map (fromJust. getFirst . mconcat . map First $) $ filter (any isJust) sdata
getInfo (LastData n) sdata = length $ filter (>n) $ map (fromJust. getLast . mconcat . map Last $) $ filter (any isJust) sdata

main = do
  fname <- head `fmap` getArgs
  sData <- getData `fmap` readFile fname
  let sData' = dataByDay sData
  putStrLn $ "NoData: " ++ (show $ getInfo NoData sData')
  putStrLn $ "AllData: " ++ (show $ getInfo AllData sData')
  putStrLn $ "AnyData: " ++ (show $ getInfo AnyData sData')
  putStrLn $ "SumData 1000: " ++ (show $ getInfo (SumData 1000) sData')
  putStrLn $ "ProductData 1000: " ++ (show $ getInfo (ProductData 1000) sData')
  putStrLn $ "FirstData 50: " ++ (show $ getInfo (FirstData 50) sData')
  putStrLn $ "LastData 50 : " ++ (show $ getInfo (LastData 50) sData')
