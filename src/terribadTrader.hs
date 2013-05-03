import Types
import Data.Maybe
-- momentum :: OrderBookEntryList -> average -> numShares -> money -> returns (OrderBookEntryList, numShares, money)
--momentum :: [OrderBookEntry] -> Float -> Integer -> Float -> ([OrderBookEntry], Integer, Float)
--momentum oBookEntryList average shares money
--   | average > minSell = sell
--   | average < maxBuy = buy

-- calcAverage :: orderBookEntryList -> currentIndex -> newAverage
calcAverage :: [OrderBookEntry] -> Integer -> Float
calcAverage oBookEntryList index = (sum' 10 index oBookEntryList) / 10
--   | index > 10 && (length oBookEntryList) > index = (sum' 10 index oBookEntryList) `div` 10
--   | index > 10 && (length oBookEntryList) < index = (sum' 10 (length oBookEntryList) oBookEntryList) `div` 10
--   | index < 10 && (length oBookEntryList) > index = 

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
sellPeak = 0.3
buyDip = 0.5

data TraderState = 
     TraderState { kn :: [OrderBookEntry],
                   momtm :: Float,
                   mony :: Float,
                   his :: [OrderBookEntry],
                   sha :: [OrderBookEntry]} 
                   deriving (Show, Read, Eq)

defaultTraderState = TraderState [] 0.0 0.0 [] []

--The function that runs a running tally of the momentum, money available and shares held.
--This function is recursive and returns the result at the end.
traderBrain :: [OrderBookEntry] -> TraderState -> TraderState
traderBrain [] result = result
traderBrain (x:allRecords) current = do
   let newKnown = x:kn current 
       newMomentum = calcAverage newKnown $ toInteger ((length newKnown) - 1)
       momentum = momtm current
       shares = sha current
       histo = his current
       money = mony current
   if abs((newMomentum - momentum)/momentum) <= epsilon then
        --We've reached a peak/valley. Buy or sell accordingly.

        --Sell everything we're worth. Assume we always succeed.
        if newMomentum >= sellPeak then traderBrain allRecords current {kn = newKnown, momtm = newMomentum, 
                                    mony = (money+shareVal(shares)), sha = [], his = histo ++ shares}

        --Buy as many shares as we can. Ideally from the cheapest source.
        else if newMomentum <= buyDip then traderBrain allRecords current {kn = newKnown, momtm = newMomentum}

        --Otherwise just don't bother.
        else traderBrain allRecords current {kn = newKnown, momtm = newMomentum}
   else
        --Simply continue. We haven't reached anything noteworthy.
        traderBrain allRecords current {kn = newKnown, momtm = newMomentum}
    where        
        shareVal orders = sum $ map (fromMaybe 0 . price) orders


buyShares :: [OrderBookEntry] -> Float -> ([OrderBookEntry], Float)
buyShares [] money = ([], money)
buyShares xs money = (ys, extraMoney)
   where cheapestAsk = findCheapestAsk (head xs) x
         ys = filter (\x -> x /= cheapestAsk) xs
         cheapPrice = price cheapestAsk
         extraMoney = money - (cheapPrice * (numCanBuy cheapPrice (volume cheapestAsk) money))

numCanBuy :: Float -> Integer -> Float -> Integer
numCanBuy price amount money
   | amount >= 0 && price * amount <= money = amount
   | otherwise = numCanBuy price (amount - 5) money

findCheapestAsk :: OrderBookEntry -> [OrderBookEntry] -> OrderBookEntry
findCheapestAsk y [] = y
findCheapestAsk y [x]
   | not (isBid x) && (price x) < (price y) = x
   | otherwise = y
findCheapestAsk y (x:xs)
   | not (isBid x) && (price x) < (price y) = findCheapestAsk x xs
   | otherwise findCheapestAsk y xs
