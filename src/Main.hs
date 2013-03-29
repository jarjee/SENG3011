import System.IO
import Control.Monad
import Data.List

import Types
import Orderbook
import Trader
import Views

main = do
    handle <- openFile "../test/test.csv" ReadMode
    trades <- hGetContents $ getTrades handle
    let tradeHistory = processOrderbook trades
    let strategy = TradeBadly
    let tradeRecommendations = generateTrades tradeHistory strategy
    writeFile "output.csv" (csvRep tradeRecommendations)
    hClose handle


