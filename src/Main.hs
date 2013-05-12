import System.Environment
import System.IO
import Control.Monad
import Data.List
import qualified Data.Vector as V
import Data.Csv

import Types
--import Orderbook
import Trader
import Views

data MainInput = MainInput { inputCash :: Float,
                             buyAlg :: String,
                             sellAlg :: String} deriving (Show, Read)

main = do
    args <- getArgs
    if (length args == 4) then process args else putStrLn "Please give FileName Money BuyAlgorithm SellAlgorithm"

process :: [String] -> IO()
process arg = do
    file <- readF $ head arg
    parse (MainInput (read $ arg !! 1) (arg !! 2) (arg !! 3)) file

parse :: MainInput -> Either String (Header, V.Vector OrderBookEntry) -> IO()
parse input fields = either (putStrLn) (dataProcessing input) fields

dataProcessing :: MainInput -> (Header, V.Vector OrderBookEntry) -> IO()
dataProcessing input (head, fields) = do
    let allRecords = V.toList fields
        cash = inputCash input
        tradeRecords = traderEntry $ allRecords
        tradeResult = traderBrain tradeRecords $ defaultTraderState {money = cash}
    putStrLn $ "The Trader was given : $"++show(cash)
    putStrLn $ "The Trader ended up with:"
    putStrLn $ '$':show(money tradeResult)
    putStrLn $ "Holding "++show(length $ sha tradeResult)++" shares."-- ++show((sum $ map (\x -> (shaPri x) * fromInteger (shAmt x)) $ sha tradeResult))
    putStrLn $ "And had sold "++show(length $ his tradeResult)++" shares."-- ++show((sum $ map (\x -> (shaPri x) * fromInteger (shAmt x)) $ his tradeResult))


--main = do
--    handle <- openFile "../test/test.csv" ReadMode
--    trades <- fmap getTrades $ hGetContents handle
--    let tradeHistory = processOrderbook trades
--    let strategy = TradeBadly 50
--    let tradeRecommendations = generateTrades tradeHistory strategy
--    writeFile "output.csv" (csvRep tradeRecommendations)
--    hClose handle
