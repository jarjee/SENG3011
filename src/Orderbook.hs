module Orderbook (processOrderbook, splitOrders) where

import Types
import Data.List


-- enter :: someData - > Order   -- submitOrder
-- amend :: Int -> Order -> Order    -- amendOrder = ammendVol | amendPrice
-- deleteOrder :: Order -> Nothing --not Haskell nothing just something that is nothing, I'm a monkey not a Guru remember! :P

------------------------------------------------------------------------------

-- enterOrder = OrderBookEntry (a million things)
-- add to list of all orders
-- process orders

-- amendOrder :: OrderBookEntry -> Float -> OrderBookEntry | OrderBookEntry -> Integer -> OrderBookEntry
amendOrder = undefined
-- add to order list
-- process orderbook

deleteOrder = undefined
-- remove from list/orderbook
-- do nothing


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
