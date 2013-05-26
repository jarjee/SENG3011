{-# LANGUAGE DoAndIfThenElse #-}
module Orderbook where
import Types as T
import Trader
import Data.HashMap as M
import Data.Heap as H
import Data.Vector as V
import Control.Applicative

averageRes = 10

data OrderBookState = OrderBookState 
       { entriesNum :: Integer,
        average :: Double,
        lastSamples :: Vector Double,
        buyRecords :: Map Integer OrderBookEntry,
        sellRecords :: Map Integer OrderBookEntry,
        buyPrices :: MaxPrioHeap Double OrderBookEntry,
        sellPrices :: MinPrioHeap Double OrderBookEntry }
        deriving (Eq, Show)

data BidKey = BidKey { bidPrice :: Double, bidTime :: String} deriving (Eq, Show, Ord)

defaultOrderBookState = OrderBookState 0 0 V.empty M.empty M.empty H.empty H.empty

{-
    We need a constant to identify the trader with.
    It's possible to come up with a better form of identification
    but since this is only a simple project it's 
    what we're going to use
-}
traderId = 42

{- 
    This is going to be rather tricky to implement given
    that we need to make concessions for the trader.

    We also need two different sorting algorithms,
    one for price for the asks, one with (price+time) for
    the bids.

    The best idea I have is pretty much linearly
    match trades (only going down as long as the longest bid/ask list),
    where we rely on updateOrderBook to make sure the lists we use
    are up to date and sorted.

    Doing the sort and then the search is O(n + nlog(n)), which might
    be a bit expensive for large values of n. (Given that we call this
    every time we enter a new value!)

    I might want to look into trying to get the sorted values in the
    maps instead, which would make life much easier + speed up
    insertion somewhat.

    VERY IMPORTANT: Due to how I handwave away the sorts, it'd be in my best
    interest to make sure that the lists do not store any values which do
    not store a price, given that it'd skew the results drastically.

-}

matchTrades :: OrderBookState -> OrderBookState
matchTrades state = state

{- For BuyPromise we need to build a new Bid to trade with
   Since I assume that when we succeed in a trade for one of our traders
   results (when I get trade matching working) we return an ask which the
   trader then hoards until needed.

   Fuffiling the AskPromise is thus just adding it to the pool.
-}
fuffilPromises :: TraderPromise -> OrderBookState -> OrderBookState
fuffilPromises (BuyPromise vol core) state = state
fuffilPromises (AskPromise entry) state = state

{-
    Currently we make some estimations which affect the accuracy of this
    simulation. Enter is fine, as is delete, but amend is supposed to change
    the order of the entries and trade is supposed to be generated internally,
    not actually processed by our trading algorithm
-}
updateOrderBook :: OrderBookEntry -> OrderBookState -> OrderBookState
updateOrderBook entry state
    | typ == T.enter = enterOrderBook entry result
    | typ == T.delete = deleteOrderBook entry result
    | typ == T.amend = amendOrderBook entry result
    | typ == T.trade = tradeOrderBook entry result
    | otherwise = state
    where
        newEntries = (entriesNum state)+1
        result = calculateAverage entry state {entriesNum = newEntries}
        typ = recordType entry

enterOrderBook :: OrderBookEntry -> OrderBookState -> OrderBookState
enterOrderBook entry state = do
    let idNum = getId entry
        entryPri = maybe (0) (id) (price entry)
    if (isBid entry) then do
        let newMap = M.insert idNum entry $ buyRecords state
            newHeap = H.insert (entryPri, entry) $ buyPrices state
        state {buyRecords = newMap, buyPrices = newHeap}
    else do
        let newMap = M.insert idNum entry $ sellRecords state
            newHeap = H.insert (entryPri, entry) $ sellPrices state
        state {sellRecords = newMap, sellPrices = newHeap}

deleteOrderBook :: OrderBookEntry -> OrderBookState -> OrderBookState
deleteOrderBook entry state = do
    let idNum = getId entry
    if (isBid entry) then do
        let newMap = M.delete (getId entry) (buyRecords state)
            newHeap = makePriceHeap newMap
        state {buyRecords = newMap, buyPrices = newHeap}
    else do
        let newMap = M.delete (getId entry) (sellRecords state)
            newHeap = makePriceHeap newMap
        state {sellRecords = newMap, sellPrices = newHeap}

amendOrderBook :: OrderBookEntry -> OrderBookState -> OrderBookState
amendOrderBook entry state = do
    let idNum = getId entry
    if (isBid entry) then do
        let newMap = M.insert idNum entry $ buyRecords state
            newHeap = makePriceHeap newMap
        state {buyRecords = newMap, buyPrices = newHeap}
    else do
        let newMap = M.insert idNum entry $ sellRecords state 
            newHeap = makePriceHeap newMap 
        state {sellRecords = newMap, sellPrices = newHeap}

makePriceHeap newMap = H.fromList $ Prelude.map (\(x,y) -> (maybe (0) (id) (price y),y)) $ M.toList newMap

calculateAverage :: OrderBookEntry -> OrderBookState -> OrderBookState
calculateAverage entry state = do
    let lenVal = V.length $ lastSamples state
        val = maybe 0 id (price $ entry)
        removeVal = V.last $ lastSamples state
        newVector = V.cons val $ if (lenVal >= averageRes) then V.init $ lastSamples state else lastSamples state
        divisor = fromIntegral averageRes
        lenDiv = fromIntegral lenVal
        result = state {lastSamples = newVector}
    if (lenVal < averageRes) then do
        let newAverage = ((average state) * (lenDiv - 1) + val)/if (lenDiv > 0) then lenDiv else 1
        result {average = newAverage}
    else do
        let newAverage = (average state) - (removeVal/divisor) + (val/divisor)
        result {average = newAverage}

-- for the moment even if there's left over volume just delete
-- gets passed OrderBookEntry and OrderBookState
-- guaranteed to always be given a trade
tradeOrderBook :: OrderBookEntry -> OrderBookState -> OrderBookState
tradeOrderBook entry state = do
    let aID = maybe (0) (id) $ askId <$> transContents <$> trans entry
        bID = maybe (0) (id) $ bidId <$> transContents <$> trans entry
        newBMap = M.delete bID (buyRecords state)
        newAMap = M.delete aID (sellRecords state)
        newBHeap = makePriceHeap newBMap
        newAHeap = makePriceHeap newAMap
    state {buyRecords = newBMap, buyPrices = newBHeap, sellRecords = newAMap, sellPrices = newAHeap}

calculateSpread :: OrderBookState -> Double
calculateSpread state = spread where
    topBid = maybe (fst (0, 0)) (fst) $ viewHead (buyPrices state)
    topAsk = maybe (fst (0, 0)) (fst) $ viewHead (sellPrices state)
    spread = topBid - topAsk

-- calculatePriceStep :: OrderBookState -> Double

-- | This function allows us to compare prices of records with one another for sorting
priceComp fst snd
     | (maybe (99999) (id) (price fst))  < (maybe (99999) (id) (price snd)) = LT
     | (maybe (99999) (id) (price fst)) == (maybe (99999) (id) (price snd)) = EQ
     | (maybe (99999) (id) (price fst))  > (maybe (99999) (id) (price snd)) = GT

priceTimeComp fst snd
   | priceComp fst snd == EQ && (time fst) > (time snd) = LT
   | otherwise = priceComp fst snd 

invertOrd o
    | o == LT = GT
    | o == EQ = EQ
    | o == GT = LT
