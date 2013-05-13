module Trader (
    traderEntry, TraderState(money,his,sha),
    defaultTraderState, traderBrain, Share(shAmt,shaPri))
  where

import Types
import Data.Maybe
import Debug.Trace
import Data.List as L

-- |This calculates a given average for a list of OrderBook Entries.
-- We take in a float to allow us to calculate the moving average
-- instead of needing to constantly recalculate from the entire list.
-- The integer is the index of the 'average' that needs to be calculated.
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

{- |Takes in raw list of OrderBookEntry, filters so we only have ENTER entries.
    This is strictly a hack for getting the Trader submission in a useable state.
    Don't think we need to check for only unique entries (this is n^2, so I'd rather not)
-}
traderEntry :: [OrderBookEntry] -> [OrderBookEntry]
traderEntry list = filter ((== enter) . recordType) list

-- |This is the constant used to deal with floating point errors when calculating the second derative.
epsilon = 0.001

data TraderState = 
     TraderState { kn :: [OrderBookEntry],
                   knAsks :: [OrderBookEntry],
                   knLength :: Integer,
                   momtm :: Float,
                   money :: Float,
                   his :: [Share],
                   sha :: [Share]} 
                   deriving (Show, Eq)

-- |Initialises a default traderState object so you don't have to.
defaultTraderState = TraderState [] [] 0 0.0 0.0 [] []

{- |This function handles the decision tree for given gradients.
    More of the buying/selling logic can be seperated out, which will
    not only improve code legibility but fuffil the requirement of
    having multiple trading strategies within the code.
-}
traderBrain :: [OrderBookEntry] -> TraderState -> TraderState
traderBrain [] result = result
traderBrain (x:allRecords) current = do
   let newKnown = (kn current) ++ [x]
       newLength = (knLength current) + 1
       momentum = momtm current
       newAskList = if (isBid (trans x) /= True) then sortBy (priceComp) (x:knAsks current) else knAsks current

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
                bouSha :: Share} deriving (Show, Eq)

data Share = Share { shAmt :: Integer,
        shaPri :: Float } deriving (Show, Read, Eq)

{-| This is a simple buying strategy that takes in a list of Asks and attempts buy as many items as possible.
    It assumes that it can buy as many records as it wants and in its current form does not generate trade signals.
    It also removes records once it finds a suitable match. The final version of this function will need to update
    the entries according to the volume bought.
-}
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
                remainingList = L.delete bestPrice list 
            BoughtShares remainingList remainingMoney $ Share (toInteger canBuy) bestPriceCost
        else BoughtShares list money $ Share 0 0
    else BoughtShares list money $ Share 0 0 

-- | This function allows us to compare prices of records with one another for sorting
priceComp fst snd
     | (maybe (99999) (id) (price fst))  < (maybe (99999) (id) (price snd)) = LT
     | (maybe (99999) (id) (price fst)) == (maybe (99999) (id) (price snd)) = EQ
     | (maybe (99999) (id) (price fst))  > (maybe (99999) (id) (price snd)) = GT
