--This will contain the Orderbook simulation code
module Orderbook (processOrderbook) where

-- Functions related to changing data in orders
-- three functions:
-- enter :: someData - > Order   -- submitOrder
-- amend :: Int -> Order -> Order    -- amendOrder = ammendVol | amendPrice
-- deleteOrder :: Order -> Nothing --not Haskell nothing just something that is nothing, I'm a monkey not a Guru remember! :P

------------------------------------------------------------------------------

enterOrder = undefined
-- add to bid/ask list depending on type

amendOrder = undefined
-- add to bid/ask 
-- reprocess orderbook

deleteOrder = undefined


------------------------------------------------------------------------------
-------- PROCESSING OF ORDER BOOK ENTRIES TO FORM A SORTED ORDERBOOK ---------
------------------------------------------------------------------------------

-- STEP 1: Split list of orders into bids and asks

splitOrders :: [OrderBookEntry] -> ([OrderBookEntry], [OrderBookEntry])
splitOrders [] = ([], [])
splitOrders (x : xs)
                     | (x . bidAsk == "B"    = (x : bid, ask)
                     | otherwise = (bid, x : ask)
                   where
                     ~(bid, ask) = splitList p xs

-------------------------------------------------------------------------------

-- STEP 2: Sort orders on time and price criteria

sortBidsAsks :: ([OrderBookEntry], [OrderBookEntry]) -> ([OrderBookEntry], [OrderBookEntry])
sortBidsAsks orders = sortBids (fst orders), 
                     sortAsks (snd orders)

sortBids :: [OrderBookEntry] -> [OrderBookEntry]
sortBids bids = sortBy bidOrdering (sortBy (compare `on` time) bids)

bidOrdering a b      | a > b = GT
                     | otherwise LT

sortAsks :: [OrderBookEntry] -> [OrderBookEntry]
sortAsks asks = sortBy askOrdering (sortBy (compare `on` time) asks)

askOrdering a b      | a < b = GT
                     | otherwise LT

---------------------------------------------------------------------------------

-- STEP 3: Calculate stats (spread and price step)

calculateSpread :: ([OrderBookEntry], [OrderBookEntry]) -> Float
calculateSpread orders = abs ( ( (head (fst orders) ) . price ) 
                     - ( (head (snd orders) ) . price ) )

calculatePriceStep :: ([OrderBookEntry], [OrderBookEntry]) -> Float
calculatePriceStep = 1
-- still not 100% sure what this step business is

---------------------------------------------------------------------------------

-- STEP 4: Bring it all together into an OrderBook

processOrderBook :: [OrderBookEntry] -> OrderBook
processOrderbook orders = OrderBook       (sortBidsAsks (splitOrders orders))
                                          calculateSpread (sortBidsAsks (splitOrders orders))
                                          calculatePriceStep (sortBidsAsks (splitOrders orders))
------------------------------------------------------------------------------------
