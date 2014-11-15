{-
1. Написать программу, работа которой управляется конфигурационным файлом, содержащим строки следующего формата:
имя поля=значение
Возможными именами полей являются summand (слагаемое), multiplier (множитель), divisor (делитель). Все значения
являются целыми числами. В качестве параметров командной строки программе подаются имя конфигурационного файла
и имя текстового файла с целочисленными данными. Над каждым целым числом из второго файла выполняются операции,
указанные в конфигурационном файле, то есть число складывается, умножается и делится соответственно.
Если какое-либо поле отсутствует, то действие не выполняется. Результаты вычислений выводятся на консоль.
Организовать доступ к параметрам конфигурационного файла средствами монады Reader.
-}
import Control.Monad.Reader
import System.Environment
import Data.Maybe

data Config = Config {
  summand :: Int 
  , multiplier :: Int
  , divisor :: Int} deriving Show

loadConfig :: FilePath -> IO Config
loadConfig fname = do
  content <- readFile fname
  let xs = map (\str -> span (/= '=') str) $ lines content
  let s = tail $ fromMaybe "0" $ lookup "summand" xs
  let m = tail $ fromMaybe "1" $ lookup "multiplier" xs
  let d = tail $ fromMaybe "1" $ lookup "divisor" xs
  return (Config (read s) (read m) (read d))

{-
doWork :: String -> Reader Config String
doWork str = do
  (Config s m d) <- ask
  return $ show s ++ " !! " ++ show m ++ " !! " ++ show d ++ " !! " ++ str

main = do
  (x0 : x1 : []) <- getArgs
  content <- readFile x1
  loadConfig x0 >>= print . runReader (doWork content)-}
