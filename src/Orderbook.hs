module Orderbook (processOrderbook, splitOrders) where

import Types
import Data.List
import Data.Function

-----------------------------------------------------------------------------
------------------- IMPORTANT NOTES FOR THIS MODULE -------------------------
-----------------------------------------------------------------------------



-----------------------------------------------------------------------------
------------------- AMMEND AN ORDER BOOK ENTRY ------------------------------
-----------------------------------------------------------------------------

-- TYPE 1: price

-- ammendOrderPrice :: OrderBook -> Integer -> Float -> OrderBook
amendOrderPrice orderbook orderID newPrice  = undefined

-- runs compare over each order in the bid and ask lists
findVolOrder orderbook price orderID = do
										map (comparePrice price orderID) (fst (orderbook.orders) )
										map (comparePrice price orderID) (snd (orderbook.orders) )

-- compare order id to order to see if its the one				
comparePrice price orderID order = if (order . transID == orderID)
									then ammendPrice order price	
									else order
							
-- set old price to current price, set price to new price
ammendPrice order newPrice = do
								(order . oldPrice) = (order. price)
								(order . price) = newPrice

--------------------------------------------------------------------------------

-- TYPE 2: volume									

-- ammendOrderVolume :: OrderBook -> Integer -> Integer -> OrderBook
amendOrderVolume orderbook orderID newVol = undefined

-- runs compare over each order in the bid and ask lists
findVolOrder orderbook vol orderID = do
										map (compareVol vol orderID) (fst (orderbook.orders) )
										map (compareVol vol orderID) (snd (orderbook.orders) )

-- compare order id to order to see if its the one				
compareVol vol orderID order = if (order . transID == orderID)
								then ammendVol order vol	
								else order

-- set old volume to current volume, set volume to new volume
ammendVol order newVol = do
							(order . oldVolume) = (order. volume)
							(order . volume) = newVol

								
--------------------------------------------------------------------------------
--------------FUNCTIONS COMMON TO AMMENDVOLUME AND AMMENDPRICE------------------
--------------------------------------------------------------------------------									
																	
-- sort ammended orderbook
sortNewOrder orderbook = processOrderBook (fst (orderbook . orders) ++ snd (orderbook . orders) )

-------------------------------------------------------------------------------------
---------------------------- DELETE AN ORDER BOOK ENTRY -----------------------------
-------------------------------------------------------------------------------------

deleteOrder = undefined
-- find orderID in orderbook
-- set all values except id to nothingness









------------------------------------------------------------------------------
-------- PROCESSING OF ORDER BOOK ENTRIES TO FORM A SORTED ORDERBOOK ---------
------------------------------------------------------------------------------

-- STEP 1: Split list of orders into bids and asks

splitOrders :: [OrderBookEntry] -> ([OrderBookEntry], [OrderBookEntry])
splitOrders [] = ([], [])
splitOrders (x : xs)
                     | ((x . bidAsk) == "B")    = (x : bid, ask)
                     | otherwise = (bid, x : ask)
                   where
                     ~(bid, ask) = splitOrders xs

-------------------------------------------------------------------------------

-- STEP 2: Sort orders on time and price criteria

sortBidsAsks :: ([OrderBookEntry], [OrderBookEntry]) -> ([OrderBookEntry], [OrderBookEntry])
sortBidsAsks orders = ( sortBids (fst orders), 
                     					sortAsks (snd orders) )

sortBids :: [OrderBookEntry] -> [OrderBookEntry]
sortBids bids = sortBy bidOrdering (sortBy (compare `on` (time bids)) bids)

bidOrdering a b      | (price a) > (price b) = GT
                     | otherwise = LT

sortAsks :: [OrderBookEntry] -> [OrderBookEntry]
sortAsks asks = sortBy askOrdering (sortBy (compare `on` (time asks)) asks)

askOrdering a b      | (price a) < (price b) = GT
	| otherwise = LT

-------------------------------- -------------------------------------------------

-- STEP 3: Calculate stats (spread and price step)

calculateSpread :: ([OrderBookEntry], [OrderBookEntry]) -> Float
calculateSpread orders = abs ( ( (head (fst orders) ) . price ) 
                     - ( (head (snd orders) ) . price ) )

calculatePriceStep :: ([OrderBookEntry], [OrderBookEntry]) -> Float
calculatePriceStep = 1
-- still not 100% sure what this step business is

---------------------------------------------------------------------------------

-- STEP 4: Bring it all together into an OrderBook

-- processOrderBook :: [OrderBookEntry] -> OrderBook
processOrderbook orders = OrderBook       (sortBidsAsks (splitOrders orders))
                                          calculateSpread (sortBidsAsks (splitOrders orders))
                                          calculatePriceStep (sortBidsAsks (splitOrders orders))
------------------------------------------------------------------------------------
