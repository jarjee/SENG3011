import System.IO
import Control.Monad
import Data.List
import qualified Data.Vector as V
import Data.Csv

import Types
--import Orderbook
import Trader
import Views

main = do
	csv <- Types.readF "input.csv"
	parse csv

parse :: Either String (Header, V.Vector OrderBookEntry) -> IO()
parse fields = either (putStrLn) (dataProcessing) fields

dataProcessing :: (Header, V.Vector OrderBookEntry) -> IO()
dataProcessing (head, fields) = do
    let allRecords = V.toList fields
        cash = 10000
        tradeRecords = traderEntry allRecords
        tradeResult = traderBrain tradeRecords $ defaultTraderState {money = cash}
    putStrLn $ "The Trader was given : "++show(cash)
    putStrLn $ "The Trader ended up with:"
    putStrLn $ show(money tradeResult)++" in cash"
    putStrLn $ "Holding "++show(length $ sha tradeResult)++" shares."
    putStrLn $ "And had sold "++show(length $ his tradeResult)++" shares."


--main = do
--    handle <- openFile "../test/test.csv" ReadMode
--    trades <- fmap getTrades $ hGetContents handle
--    let tradeHistory = processOrderbook trades
--    let strategy = TradeBadly 50
--    let tradeRecommendations = generateTrades tradeHistory strategy
--    writeFile "output.csv" (csvRep tradeRecommendations)
--    hClose handle
