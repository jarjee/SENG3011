{-# LANGUAGE DoAndIfThenElse #-}
module Orderbook where
import Types as T
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

defaultOrderBookState = OrderBookState 0 0 V.empty M.empty M.empty H.empty H.empty

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
