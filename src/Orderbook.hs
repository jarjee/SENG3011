{-# LANGUAGE DoAndIfThenElse #-}
module Orderbook where
import Types as T
import Trader
import Data.HashMap as M
import Data.Heap as H
import Data.Vector as V
import Control.Applicative
import Data.Maybe

averageRes = 10

data OrderBookState = OrderBookState 
       { entriesNum :: Integer,
        average :: Double,
        lastSamples :: Vector Double,
        
        buyRecords :: Map Integer OrderBookEntry,
        sellRecords :: Map Integer OrderBookEntry,
        buyPrices :: MaxPrioHeap Double OrderBookEntry,
        sellPrices :: MinPrioHeap Double OrderBookEntry,
        
        tradeBid :: MaxPrioHeap BidKey OrderBookEntry,
        tradeSell :: MaxPrioHeap Double OrderBookEntry,
        tradesMade :: [OrderBookEntry],
        traderShares :: MinPrioHeap Double OrderBookEntry,
        traderMoney :: Double,
        
        orderTime :: String }
        deriving (Eq, Show)

data BidKey = BidKey { bidPrice :: Double, bidTime :: String} deriving (Eq, Show, Ord)

defaultOrderBookState :: OrderBookState
defaultOrderBookState = OrderBookState 0 0 V.empty M.empty M.empty H.empty H.empty H.empty H.empty [] H.empty 0 ""

{- 
    This is going to be rather tricky to implement given
    that we need to make concessions for the trader.

    We also need two different sorting algorithms,
    one for price for the asks, one with (price+time) for
    the bids.
-}

matchTrades :: OrderBookState -> OrderBookState
matchTrades state = do
    let headBuy = view (tradeBid state)
        headSell = view (tradeSell state)
    if (isJust headBuy && isJust headSell) then do
        let topBuy = snd $ fst $ fromJust headBuy
            topSell = snd $ fst $ fromJust headSell
        if (maybe (0) (id) $ price topBuy) >= (maybe (0) (id) $ price topSell) then do
            let topBuyVol = maybe (0) (id) $ volume topBuy
                topSellVol = maybe (0) (id) $ volume topSell
                newBuy = topBuy {volume = Just $ topBuyVol - (calcDiff topBuy topSell)}
                tradeResult = (makeTrade topBuy topSell state) 
            --Remove sell, reduce buy, generate trade 
            if (topBuyVol > topSellVol) then do
                let newMap = M.insert (getId topBuy) topBuy $ buyRecords state
                    newHeap = makePriceHeap newMap
                    newHighHeap = makePriceTimeHeap newMap
                matchTrades tradeResult {buyRecords = newMap, buyPrices = newHeap, tradeBid = newHighHeap} 
            --Remove buy, reduce sell, generate trade
            else if (topBuyVol < topSellVol) then do
                let newMap = M.insert (getId topSell) topSell $ sellRecords state
                    newHeap = makePriceHeap newMap
                    newHighHeap = makePriceHeap newMap
                matchTrades tradeResult {sellRecords = newMap, sellPrices = newHeap, tradeSell = newHighHeap}
                --Remove sell, remove buy, generate trade
            else do
               let  newSMap = M.delete (getId topSell) $ sellRecords state
                    newSHeap = makePriceHeap newSMap
                    newSHighHeap = makePriceHeap newSMap
                    newBMap = M.delete (getId topBuy) $ buyRecords state
                    newBHeap = makePriceHeap newBMap
                    newBHighHeap = makePriceTimeHeap newBMap
               matchTrades tradeResult {buyRecords = newBMap, buyPrices = newBHeap, tradeBid = newBHighHeap,sellRecords = newSMap, sellPrices = newSHeap, tradeSell = newSHighHeap} 
        else state
    else state

makeTrade :: OrderBookEntry -> OrderBookEntry -> OrderBookState -> OrderBookState
makeTrade bid ask state = state {tradesMade = (result:tradesMade state), traderMoney = (traderMoney state)+(newMoney bid ask), traderShares = H.union (traderShares state) (newShares bid ask) }
          where result = bid {recordType = enter, time = (orderTime state), trans = Just (TransId ' ' $ Bid (getId bid) (Just (getId ask))), volume = Just (calcDiff bid ask)}
                newMoney bid ask = if (getId ask) == traderId then maybe (0) (id) (value ask) else 0 
                newShares bid as = if (getId bid) == traderId then H.singleton ((maybe (0) (id) $ price bid), newAsk as) else H.empty
                newAsk as = as {recordType = enter, time = (orderTime state), trans = Just (TransId 'A' $ Ask (traderId) Nothing), volume = Just (calcDiff bid ask)}  

calcDiff bid ask = (max (maybe (0) (id) $ volume bid) (maybe (0) (id) $ volume ask))

{- For BuyPromise we need to build a new Bid to trade with
   Since I assume that when we succeed in a trade for one of our traders
   results (when I get trade matching working) we return an ask which the
   trader then hoards until needed.

   Fuffiling the AskPromise is thus just adding it to the pool.
-}
fulfillPromises :: [TraderPromise] -> OrderBookState -> OrderBookState
fulfillPromises [] state = state
fulfillPromises (x:xs) state = fulfillPromises xs (fulfill x state)
    where fulfill x s = updateOrderBook (wantedShare x) s
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
        result = calculateAverage entry state {entriesNum = newEntries, orderTime= time entry}
        typ = recordType entry

enterOrderBook :: OrderBookEntry -> OrderBookState -> OrderBookState
enterOrderBook entry state = do
    let idNum = getId entry
        entryPri = maybe (0) (id) (price entry)
    if (isBid entry) then do
        let newMap = M.insert idNum entry $ buyRecords state
            newHeap = H.insert (entryPri, entry) $ buyPrices state
            newHighHeap = H.insert (BidKey entryPri (time entry), entry) $ tradeBid state
        state {buyRecords = newMap, buyPrices = newHeap, tradeBid = newHighHeap}
    else do
        let newMap = M.insert idNum entry $ sellRecords state
            newHeap = H.insert (entryPri, entry) $ sellPrices state
            newHighHeap = H.insert (entryPri, entry) $ tradeSell state
        state {sellRecords = newMap, sellPrices = newHeap, tradeSell = newHighHeap}

deleteOrderBook :: OrderBookEntry -> OrderBookState -> OrderBookState
deleteOrderBook entry state = do
    let idNum = getId entry
    if (isBid entry) then do
        let newMap = M.delete (getId entry) (buyRecords state)
            newHeap = makePriceHeap newMap
            newHighHeap = makePriceTimeHeap newMap
        state {buyRecords = newMap, buyPrices = newHeap, tradeBid = newHighHeap}
    else do
        let newMap = M.delete (getId entry) (sellRecords state)
            newHeap = makePriceHeap newMap
            newHighHeap = makePriceHeap newMap
        state {sellRecords = newMap, sellPrices = newHeap, tradeSell = newHighHeap}

amendOrderBook :: OrderBookEntry -> OrderBookState -> OrderBookState
amendOrderBook entry state = do
    let idNum = getId entry
    if (isBid entry) then do
        let newMap = M.insert idNum entry $ buyRecords state
            newHeap = makePriceHeap newMap
            newHighHeap = makePriceTimeHeap newMap

        state {buyRecords = newMap, buyPrices = newHeap, tradeBid = newHighHeap}
    else do
        let newMap = M.insert idNum entry $ sellRecords state 
            newHeap = makePriceHeap newMap 
            newHighHeap = makePriceHeap newMap
        state {sellRecords = newMap, sellPrices = newHeap, tradeSell = newHighHeap}

makePriceHeap newMap = H.fromList $ Prelude.map (\(x,y) -> (maybe (0) (id) (price y),y)) $ M.toList newMap

makePriceTimeHeap newMap = H.fromList $ Prelude.map (\(x,y) -> ((BidKey (maybe (0) (id) (price y)) (time y)),y)) $ M.toList newMap

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

        newHighBHeap = makePriceTimeHeap newBMap
        newHighAHeap = makePriceHeap newAMap
    state {buyRecords = newBMap, buyPrices = newBHeap, sellRecords = newAMap, sellPrices = newAHeap, tradeBid = newHighBHeap, tradeSell = newHighAHeap}

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
