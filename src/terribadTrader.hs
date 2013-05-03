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

--Take two entries (both bids or both asks) and work out the difference
--The concept of momentum is created from this (as the first entry comes earlier than the second)
--Potentially use time as the Y-coordinate?
difference :: OrderBookEntry -> OrderBookEntry -> Float
difference = undefined

epsilon = 0.001

--The function that runs a running tally of the momentum, money available and shares held.
--This function is recursive and returns the result at the end.
traderBrain :: [OrderBookEntry] -> [OrderBookEntry] -> Float -> Float -> [OrderBookEntry] -> ([OrderBookEntry], Float)
traderBrain [] _ _ money shares = (shares, money)
traderBrain (x:allRecords) known momentum money shares = do
   let newKnown = x:known 
       newMomentum = calcAverage newKnown ((length newKnown) - 1)
   if abs((newMomentum - momentum)/momentum) <= epsilon then
        traderBrain allRecords newKnown newMomentum money shares
   else
        traderBrain allRecords newKnown newMomentum money shares
