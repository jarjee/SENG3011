import System.IO
import Control.Monad
import Data.List
import qualified Data.Vector as V
import Data.Csv

import Types
import Orderbook
-- import Trader
import Views

main = do
	csv <- Types.readF "input.csv"
	parse csv

parse :: Either String (Header, V.Vector OrderBookEntry) -> IO()
parse fields = either (print) (filterFields) fields

filterFields :: (Header, V.Vector OrderBookEntry) -> IO()
filterFields (head, fields) = do
	Types.writeF "output.csv" $ (head, V.fromList  $ filter ((== TRADE) . recordType) $ V.toList fields)

--main = do
--    handle <- openFile "../test/test.csv" ReadMode
--    trades <- fmap getTrades $ hGetContents handle
--    let tradeHistory = processOrderbook trades
--    let strategy = TradeBadly 50
--    let tradeRecommendations = generateTrades tradeHistory strategy
--    writeFile "output.csv" (csvRep tradeRecommendations)
--    hClose handle
