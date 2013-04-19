module Orderbook (processOrderbook, amendOrderPrice, amendOrderVolume, calculateSpread) where

import Types
import Data.List
import Data.Function
import Data.Functor
import Control.Applicative
import Data.Maybe


-----------------------------------------------------------------------------
------------------- IMPORTANT NOTES FOR THIS MODULE -------------------------
-----------------------------------------------------------------------------

-- amend order does not update the time stamp at this stage
-- volume and price amends can be combined for later release (but I am not smart enough now)

-- very little checking for empty things, need to dicuss and add

-- order tuple goes (bids, asks)

-----------------------------------------------------------------------------
------------------- AMMEND AN ORDER BOOK ENTRY ------------------------------
-----------------------------------------------------------------------------

-- TYPE 1: price

-- ammendOrderPrice :: OrderBook -> Integer -> Float -> OrderBook
amendOrderPrice orderbook orderID newPrice  = undefined

-- runs compare over each order in the bid and ask lists
findPriceOrder orderbook price orderID = do
	map (comparePrice price orderID) (fst (orders orderbook) )
	map (comparePrice price orderID) (snd (orders orderbook) )

-- compare order id to order to see if its the one				
comparePrice price orderID order = if ((transId order) == orderID)
									then ammendPrice order price	
									else order
							
-- set old price to current price, set price to new price
ammendPrice order newPrice = changeNewPrice (changePrice order) newPrice
								
changePrice order = order {oldPrice = (price order)}
								
changeNewPrice order newPrice = order {price = newPrice}

--------------------------------------------------------------------------------

-- TYPE 2: volume									

-- ammendOrderVolume :: OrderBook -> Integer -> Integer -> OrderBook
amendOrderVolume orderbook orderID newVol = undefined

-- runs compare over each order in the bid and ask lists
findVolOrder orderbook vol orderID = do
										map (compareVol vol orderID) (fst (orders orderbook) )
										map (compareVol vol orderID) (snd (orders orderbook) )

-- compare order id to order to see if its the one				
compareVol vol orderID order = if ((transId order) == orderID)
								then ammendVol order vol	
								else order

-- set old volume to current volume, set volume to new volume
ammendVol order newVol = changeNewVol (changeVol  order) newVol

changeVol order = order {oldVolume = (volume order)}
							
changeNewVol order newVol = order {volume = newVol}

								
--------------------------------------------------------------------------------
--------------FUNCTIONS COMMON TO AMMENDVOLUME AND AMMENDPRICE------------------
--------------------------------------------------------------------------------									
																	
-- sort ammended orderbook
sortNewOrder orderbook = processOrderbook (fst (orders orderbook) ++ snd (orders orderbook) )

-------------------------------------------------------------------------------------
---------------------------- DELETE AN ORDER BOOK ENTRY -----------------------------
-------------------------------------------------------------------------------------

--clearOrderBook orderId orderbook = deleteOrder orderId (snd (orders (deleteOrder orderId (fst (orders orderbook))) ) )

--deleteOrder :: Integer -> [OrderBookEntry] -> [OrderBookEntry]
--deleteOrder orderId orders = filter ((/= orderId) . transId) orders

------------------------------------------------------------------------------
-------- PROCESSING OF ORDER BOOK ENTRIES TO FORM A SORTED ORDERBOOK ---------
------------------------------------------------------------------------------

-- STEP 1: Split list of orders into bids and asks

splitOrders :: [OrderBookEntry] -> ([OrderBookEntry], [OrderBookEntry])
splitOrders [] = ([], [])
splitOrders (x : xs)
	|(fmap (isBid) $ trans x) == (Just True) = (x : bid, ask)
	|(fmap (isBid) $ trans x) == (Just False) = (bid, x : ask)
	|otherwise = (bid, ask)
             where
               (bid, ask) = splitOrders xs
	
isBid :: TransId -> Bool
isBid (Bid _ _) = True
isBid (Ask _ _) = False

-------------------------------------------------------------------------------

-- STEP 2: Sort orders on time and price criteria

sortBidsAsks :: ([OrderBookEntry], [OrderBookEntry]) -> ([OrderBookEntry], [OrderBookEntry])
sortBidsAsks orders = ( sortBids (fst orders), 
                     					sortAsks (snd orders) )

sortBids :: [OrderBookEntry] -> [OrderBookEntry]
sortBids bids = sortBy bidOrderingPrice (sortBy bidOrderingTime bids)

bidOrderingPrice a b    | (price a) > (price b) = GT
						| otherwise = LT
bidOrderingTime a b   	| (time a) < (time b) = GT
						| otherwise = LT
						
						
sortAsks :: [OrderBookEntry] -> [OrderBookEntry]
sortAsks asks = sortBy askOrderingPrice (sortBy askOrderingTime asks) 

askOrderingPrice a b    | (price a) < (price b) = GT
						| otherwise = LT
askOrderingTime a b   	| (time a) < (time b) = GT
						| otherwise = LT

-------------------------------- -------------------------------------------------

-- STEP 3: Calculate stats (spread and price step)

calculateSpread :: ([OrderBookEntry], [OrderBookEntry]) -> Float
calculateSpread orders
	| ((-) <$> (price bid) <*> (price ask)) == Nothing = 0
	| otherwise = abs(fromJust $ (-) <$> (price bid) <*> (price ask))
	where bid = head $ fst orders
	      ask = head $ snd orders

calculatePriceStep :: ([OrderBookEntry], [OrderBookEntry]) -> Float
calculatePriceStep = undefined
-- still not 100% sure what this step business is

---------------------------------------------------------------------------------

-- STEP 4: Bring it all together into an OrderBook

-- processOrderBook :: [OrderBookEntry] -> OrderBook
processOrderbook orders = OrderBook (sortBidsAsks (splitOrders orders))
										(calculateSpread (sortBidsAsks (splitOrders orders)))
											(calculatePriceStep (sortBidsAsks (splitOrders orders)))
------------------------------------------------------------------------------------
