--This will contain the Orderbook simulation code
module Orderbook (processOrderbook) where

-- This is all for defining a new type for orders (maybe else where??)
-- Order data type
-- BidOrder, AskOrder??

--type Instr = String
--type Date = crazy Haskell date type
--type Time = System.Time  
--type RecType = String -- "enter", "amend", "delete"
--type Price = Float
--type Volume = Int
--type Value = Int
--type TransID = Int ?
--type BidID = Int
--type AskId = Int
--type OrderType = OrdType -- OrdType = Bid | Ask

-- data Order = Order Instr Date Time RecType 
                     -- Price Volume Value TransID BidID AskID OrderType


-- Functions related to changing data in orders
-- three functions:
-- enter :: someData - > Order   -- submitOrder
-- amend :: Int -> Order -> Order    -- amendOrder = decreaseVol | IncreaseVol | amendPrice
-- deleteOrder :: Order -> Nothing --not Haskell nothing just something that is nothing, I'm a monkey not a Guru remember! :P


-- Actually process the order book
processOrderbook = undefined

-- where do we get the orders from?
-- assuming orders come in from somewhere in lists of data types (bid and ask)
-- do we assume they are for one company only?
-- order asks from lowest to highest price, earliest to latest (i think)
-- order bids from highest to lowest price, earliest to latest (i think)
-- going to do more research into the process before i write more crap here

-- spread: difference in price between best bid and best ask at a time
-- calculateSpread :: Int -> OrderBook -> Float
-- input time, order book, output spread

-- price step: minimum diff in price between 2 groups of same price orders

-- we need an orderbook type too (maybe not in here though)
-- data OrderBook = OrderBook BidList AskList
-- type BidList = [Order]
-- type AskList = [Order]
-- as in lists of type Order
