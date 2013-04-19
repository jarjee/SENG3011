--This file will hold the code required to do all the trade generation
module Trader (generateTrades, Strategy(..)) where

import Types
import Orderbook
import Data.Functor
import Control.Applicative

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
								tradePrice :: Maybe Float,
								tradeVolume :: Maybe Integer,
								bidID :: Integer,
								askID :: Integer
							} deriving (Show, Eq)

------------------------------STEP 1: GET TRADE DETAILS---------------------------------

--getBestOrderPair :: orderbook -> (OrderBookEntry, OrderBookEntry)
getBestOrderPair obook = (head (fst (orders obook)), head (snd (orders obook)) )

--getVolume :: (OrderBookEntry, OrderBookEntry) -> Integer
getVolume (bid, ask)
		|((volume bid) < (volume ask)) = (volume bid)
		|((volume ask) < (volume bid)) = (volume ask)
		|otherwise = (volume bid)

--getPrice :: OrderBook -> Float
getPrice obook  
		| ((spread obook) == 0) = (price (fst (getBestOrderPair obook)) )
		| ((spread obook) < 0) = price (earliestOrder (getBestOrderPair obook))
		| otherwise = doOtherThing
		
doOtherThing = undefined
												
--earliestOrder :: (OrderBookEntry, OrderBookEntry) -> Float
earliestOrder (bid, ask)
		|((time bid) < (time ask)) = bid
		|otherwise = ask
							
--recordTradeStats :: OrderBook -> TradeStats		
recordTradeStats obook = TradeStats (getBestOrderPair obook) 
										(getPrice obook) 
											(getVolume (getBestOrderPair obook)) 
												(fst (getBestOrderPair obook))
													(snd (getBestOrderPair obook))

------------------------------STEP 2: UPDATE VOLUME VALUES----------------------------

--generateTrades :: OrderBook -> TradeLog
generateTrades orderbook = makeTrade (recordTradeStats orderbook) orderbook

--makeTrade :: TradeStats -> OrderBook -> OrderBook
makeTrade tstats obook = updateBidList tstats (updateAskList tstats obook)

--updateBidList :: TradeStats -> OrderBook -> OrderBook
updateBidList :: TradeStats -> OrderBook -> OrderBook
updateBidList tstats obook = obook {orders = ([(head (fst (orders obook))) {volume = ((volume (head (fst (orders obook)))) - (tradeVolume tstats))}] ++ (drop 1 (fst (orders obook))), (snd (orders obook)))}

--updateAskList :: TradeStats -> OrderBook -> OrderBook
updateAskList tstats obook = obook {orders = ([(head (snd (orders obook))) {volume = ((volume (head (snd (orders obook)))) - (tradeVolume tstats))}] ++ (drop 1 (snd (orders obook))), (snd (orders obook)))}

------------------------------STEP 3: DELETE USED ORDERS------------------------------------
				
--pergeOrderBook :: OrderBook -> (OrderBookEntry, OrderBookEntry)
pergeOrderBook obook = (pergeOrders (fst (orders obook)), pergeOrders (snd (orders obook)))

--pergeOrders :: [OrderBookEntry] -> [OrderBookEntry]
pergeOrders os = filter (\o -> (volume o) /= 0) os
				
------------------------------STEP : DETERMINE NEW SPREAD------------------------------------
				
--newSpread :: OrderBook -> OrderBook
--newSpread orderbook = orderbook {spread = calculateSpread (orders orderbook)}

----------------------------------------------------------------------------------------------

--Whatever you want, I've just put this in place so it can compile

data Strategy = TradeBadly Int | TradeLimit Int Int deriving (Show, Eq, Read)
