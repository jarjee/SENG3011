---CREATE ORDER BOOK---
--1. 
processOrderBook [orders] -- takes in whole csv of orders and produces and OrderBook from them



---MAKE TRADES---
--1. 
generateTrades orderbook -- returns a TradeLog (all info on trade made)
--2. 
push onto list of TradeLogs....
--3.
updateOrderBook orderbook = OrderBook (pergeOrderBook orderbook) 
								(calculateSpread (pergeOrderBook orderbook)) 
									(calculatePriceStep (pergeOrderBook orderbook))
-- this looks shit cause pergeOrderBook is fucked but its the best I can do for this deliverable
--4.
-- rinse and repeat
