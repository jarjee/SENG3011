module Trader (
    traderEntry, TraderState(money,his,sha),
    defaultTraderState, traderBrain, Share(shAmt,shaPri))
  where

import Types
import Data.Maybe
import Debug.Trace
import Data.List

-- calcAverage :: orderBookEntryList -> currentIndex -> newAverage
calcAverage :: [OrderBookEntry] -> Float -> Integer -> Float
calcAverage oBookEntryList oldAverage index 
   | index < 0  = oldAverage
   | index < 10 = (sum' 10 index oBookEntryList) / 10
   | otherwise  = oldAverage - (fromMaybe 0 (price (oBookEntryList !! (fromIntegral (index - 10)))) / 10) + (fromMaybe 0 (price (oBookEntryList !! (fromIntegral index))) / 10)

sum' :: Integer -> Integer -> [OrderBookEntry] -> Float
sum' 0 _ list = 0
sum' _ 0 list = fromMaybe 0 $ price $ (head list)
sum' amount index list
   | amount > (index + 1) = sum' (index + 1) index list
   | amount > (toInteger (length list)) = sum' (toInteger (length list)) index list
   | index >= (toInteger (length list)) = sum' amount (toInteger (length list - 1)) list
   | otherwise = (fromMaybe 0 (price (list !! (fromIntegral index)))) + (sum' (amount - 1) (index - 1) list)

--Takes in raw list of OrderBookEntry, filters so we only have ENTER entries
--Don't think we need to check for only unique entries (this is n^2, so I'd rather not)
traderEntry :: [OrderBookEntry] -> [OrderBookEntry]
traderEntry list = filter ((== ENTER) . recordType) list

epsilon = 0.001

data TraderState = 
     TraderState { kn :: [OrderBookEntry],
                   knAsks :: [OrderBookEntry],
                   knLength :: Integer,
                   momtm :: Float,
                   money :: Float,
                   his :: [Share],
                   sha :: [Share]} 
                   deriving (Show, Read, Eq)

--Initialises a default traderState object so you don't have to.
defaultTraderState = TraderState [] [] 0 0.0 0.0 [] []

--The function that runs a running tally of the momentum, currMoney available and shares held.
--This function is recursive and returns the result at the end.
traderBrain :: [OrderBookEntry] -> TraderState -> TraderState
traderBrain [] result = result
traderBrain (x:allRecords) current = do
   let newKnown = (kn current) ++ [x]
       newLength = (knLength current) + 1
       momentum = momtm current
       newAskList = if (isBid (trans x) /= True) then sortBy (lowPrice) (x:knAsks current) else knAsks current

       newMomentum = calcAverage newKnown momentum (newLength-1)
       secgradient = (calcAverage newKnown newMomentum (newLength-2))
       thrdgradient = (calcAverage newKnown secgradient (newLength-3))
       gradient = (secgradient - thrdgradient)

       resultState = current {kn = newKnown, knLength = newLength, momtm = newMomentum}
       shares = sha current
       histo = his current
       currMoney = money current

   if abs((newMomentum - momentum)/momentum) <= epsilon then
        --We've reached a peak/valley. Buy or sell accordingly.

        --Sell everything we're worth. Assume we always succeed.
        if gradient > 0.002 then do
            let sellUpdate = resultState {
                money = (currMoney+shareVal gradient (shares)), 
                sha = [], his = histo ++ shares}
            traderBrain allRecords sellUpdate

        --Buy as many shares as we can. Ideally from the cheapest source.
        else if gradient <= 0 then do
            let result = buyShares' newAskList currMoney
                updatedShares = if shAmt (bouSha result) /= 0 then resultState {sha = (bouSha result):shares, kn = remShares result, knLength = toInteger $ length $ remShares result} else resultState

            --knLength must be -1 as we remove a record when we buy
            traderBrain allRecords $ updatedShares { 
            money = (remMoney result)}

        --Otherwise just don't bother.
        else traderBrain allRecords $ resultState 
   else
        --Simply continue. We haven't reached anything noteworthy.
        traderBrain allRecords $ resultState 
    where        
        shareVal gradient orders = (sum $ map (\x -> (0.0005 + shaPri x) * fromInteger (shAmt x)) orders)

data BoughtShares = BoughtShares { remShares :: [OrderBookEntry],
                remMoney :: Float,
                bouSha :: Share} deriving (Show, Read, Eq)

data Share = Share { shAmt :: Integer,
        shaPri :: Float } deriving (Show, Read, Eq)

buyShares' :: [OrderBookEntry] -> Float -> BoughtShares
buyShares' [] money = BoughtShares [] money $ Share 0 0
buyShares' list money = do
    if list /= [] then do
        let bestPrice = head list
            bestPriceCost = maybe (0) (id) (price bestPrice)
            canBuy = truncate $ money / bestPriceCost
        if canBuy > 0 then do
            let shareCost = (fromIntegral canBuy) * bestPriceCost
                remainingMoney = money - shareCost
                remainingList = delete bestPrice list 
            BoughtShares remainingList remainingMoney $ Share (toInteger canBuy) bestPriceCost
        else BoughtShares list money $ Share 0 0
    else BoughtShares list money $ Share 0 0 
    
lowPrice fst snd
     | (maybe (99999) (id) (price fst))  < (maybe (99999) (id) (price snd)) = LT
     | (maybe (99999) (id) (price fst)) == (maybe (99999) (id) (price snd)) = EQ
     | (maybe (99999) (id) (price fst))  > (maybe (99999) (id) (price snd)) = GT

--buyShares :: [OrderBookEntry] -> Float -> BoughtShares
--buyShares [] money = BoughtShares [] money $ Share 0 0
--buyShares xs money = BoughtShares ys extraMoney $ Share volToBuy cheapPrice
--   where cheapestAsk = findCheapestAsk (head xs) xs
--         ys = filter (\x -> x /= cheapestAsk) xs
--         cheapPrice = fromMaybe 999999 (price cheapestAsk)
--         volToBuy = numCanBuy cheapPrice (fromMaybe 0 (volume cheapestAsk)) money
--         extraMoney = money - (cheapPrice * (fromInteger volToBuy))
--
--numCanBuy :: Float -> Integer -> Float -> Integer
--numCanBuy price amount money
--   | amount >= 0 && price * (fromInteger amount) <= money = amount
--   | otherwise = numCanBuy price (amount - 5) money

--findCheapestAsk :: OrderBookEntry -> [OrderBookEntry] -> OrderBookEntry
--findCheapestAsk y [] = y
--findCheapestAsk y [x]
--   | not (isBid (fromMaybe (Bid 0 0) (trans x))) && (price x) < (price y) = x
--   | otherwise = y
--findCheapestAsk y (x:xs)
--   | not (isBid (fromMaybe (Bid 0 0) (trans x))) && (price x) < (price y) = findCheapestAsk x xs
--   | otherwise = findCheapestAsk y xs
