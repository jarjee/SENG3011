import System.IO
import Control.Monad
import Data.List

import Types
import Orderbook
import Trader
import Views

data OrderBookEntry = 
        OrderBookEntry { instrument :: String,
                         date :: String,
                         time :: String,
                         recordType :: String,
                         price :: String,
                         volume :: String,
                         undisclosedVolume :: String,
                         value :: String,
                         qualifiers :: String,
                         transId :: String,
                         bidId :: String,
                         askId :: String,
                         entryTime :: String,
                         oldPrice :: String,
                         oldVolume :: String,
                         buyerBrokerId :: String,
                         sellerBrokerId :: String
        }

main = do
    handle <- openFile "../test/test.csv" ReadMode
    trades <- getTrades handle
    let tradeHistory = processOrderbook trades
    let strategy = TradeBadly
    let tradeRecommendations = generateTrades tradeHistory strategy
    writeFile "output.csv" (csvRep tradeRecommendations)
    hClose handle

split :: (a -> Bool) -> [a] -> [[a]]
split pr [] = []
split pr l = h : (split pr f)
    where
        (h,t) = break pr l
        f = if null t then [] else tail t

dropPred :: (a -> Bool) -> [a] -> [a]
dropPred _ [] = []
dropPred pr (x:xs) = if pr x then xs else x:dropPred pr xs
