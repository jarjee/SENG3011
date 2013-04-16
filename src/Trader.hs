--This file will hold the code required to do all the trade generation
module Trader (generateTrades, Strategy(..)) where

import Types
import Orderbook

-----------------------------------------------------------------------------
------------------- IMPORTANT NOTES FOR THIS MODULE -------------------------
-----------------------------------------------------------------------------

-- Three functions must be called here to succesfully generate one trade
-- 1. generateTrades
-- 2. pergeOrderBook
-- 3. newSpread
-- eventually returning an up-to-date orderbook
-- call this series recursively for full trading

-- have introduced a new data type which could be accumulated into a list of trades
-- for reporting purposes

-------------------------------------------------------------------------------

data TradeStats =
				TradeStats { 	trade :: (OrderBookEntry, OrderBookEntry),
								price :: Float,
								volume :: Integer,
								bidID :: Integer,
								askID :: Integer
							} deriving (Show, Eq)

------------------------------STEP 1: GET TRADE DETAILS---------------------------------

getBestOrderPair :: orderbook -> (OrderBookEntry, OrderBookEntry)
getBestOrderPair obook = (head (fst (orders obook)), head (snd (orders obook)) )

getVolume :: (OrderBookEntry, OrderBookEntry) -> Integer
getVolume (bid, ask)
		|((volume bid) < (volume ask)) = (volume bid)
		|((volume ask) < (volume bid)) = (volume ask)
		|otherwise = (volume bid)

getPrice :: OrderBook -> Float
getPrice obook = if (spread obook) <= 0
							then 
								if (spread orderbook) == 0
								then (price (fst (getBestOrderPair obook)) )
									else earliestOrder (getBestOrderPair obook)
												
earliestOrder :: (OrderBookEntry, OrderBookEntry) -> Float
earliestOrder (bid, ask)
						|((time bid) < (time ask)) = (time bid)
						|otherwise = (time ask)
							
recordTradeStats :: OrderBook -> TradeStats		
recordTradeStats obook = TradeStats (getBestOrderPair obook) (getPrice obook) 
										(getVolume (getBestOrderPair obook)) (fst (getBestOrderPair obook))
											(snd (getBestOrderPair obook))

------------------------------STEP 2: UPDATE VOLUME VALUES----------------------------

generateTrades :: OrderBook -> TradeLog
generateTrades orderbook = makeTrade (recordTradeStats orderbook) orderbook

makeTrade :: TradeStats -> OrderBook -> OrderBook
makeTrade tstats obook = updateBidList tstats (updateAskList tstats obook)

updateBidList :: TradeStats -> OrderBook -> OrderBook
updateBidList tstats obook = (head (fst (order obook))) {volume = (volume - (volume tstats)}

updateAskList :: TradeStats -> OrderBook -> OrderBook
updateBidList tstats obook = (head (snd (order obook))) {volume = (volume - (volume tstats)}

------------------------------STEP 3: DELETE USED ORDERS------------------------------------
				
pergeOrderBook :: OrderBook -> OrderBook
pergeOrderBook obook = pergeOrders (snd orders (pergeOrders (fst (orders obook)) ) )

pergeOrders :: [OrderBookEntry] -> [OrderBookEntry]
pergeOrders os = filter (\o -> (volume o) > 0) os
				
------------------------------STEP : DETERMINE NEW SPREAD------------------------------------
				
newSpread :: OrderBook -> OrderBook
newSpread orderbook = orderbook {spread = Orderbook.calculateSpread (orders orderbook)}

----------------------------------------------------------------------------------------------

--Whatever you want, I've just put this in place so it can compile

data Strategy = TradeBadly Int | TradeLimit Int Int deriving (Show, Eq, Read)
