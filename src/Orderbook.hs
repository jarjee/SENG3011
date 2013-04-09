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

-------------------------------------------------------------------------------

-- Actually process the order book
processOrderbook = undefined
-- call ordering functions
-- add spread and step data

-------------------------------------------------------------------------------
sortBids :: BidList -> BidList
sortBids bids = sortBy (>)(sortBy (compare `on` time) bids)


--------------------------------------------------------------------------------
sortAsks :: AskList -> AskList
sortAsks = undefined
sortAsks asks = sortBy (<) (sortBy (compare `on` time) asks)


--------------------------------------------------------------------------------

createOrderBook :: BidList -> AskList -> OrderBook
createOrderBook  = undefined

--------------------------------------------------------------------------------

calculateSpread :: BidList -> AskList -> Float
calculateSpread a b = abs ( ( (head a) . price ) 
                     - ( (head b) . price ) )

---------------------------------------------------------------------------------

-- still not 100% sure what this step business is
calculatePriceStep :: OrderBook -> Float
calculatePriceStep = undefined

---------------------------------------------------------------------------------

-- do we assume they are for one company only?

-- price step: minimum diff in price between 2 groups of same price orders

-- we need an orderbook type too (maybe not in here though)
-- data OrderBook = OrderBook BidList AskList
-- type BidList = [Order]
-- type AskList = [Order]
-- as in lists of type Order
