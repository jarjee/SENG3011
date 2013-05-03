import Types
-- momentum :: OrderBookEntryList -> average -> numShares -> money -> returns (OrderBookEntryList, numShares, money)
--momentum :: [OrderBookEntry] -> Float -> Integer -> Float -> ([OrderBookEntry], Integer, Float)
--momentum oBookEntryList average shares money
--   | average > minSell = sell
--   | average < maxBuy = buy

-- calcAverage :: orderBookEntryList -> currentIndex -> newAverage
calcAverage :: [OrderBookEntry] -> Integer -> Float
calcAverage oBookEntryList index = (sum' 10 index oBookEntryList) `div` 10
--   | index > 10 && (length oBookEntryList) > index = (sum' 10 index oBookEntryList) `div` 10
--   | index > 10 && (length oBookEntryList) < index = (sum' 10 (length oBookEntryList) oBookEntryList) `div` 10
--   | index < 10 && (length oBookEntryList) > index = 

sum' :: Integer -> Integer -> [OrderBookEntry] -> Float
sum' 0 _ list = 0
sum' _ 0 list = price (head list)
sum' take index list
   | take > (index + 1) = sum' (index + 1) index list
   | take > (length list) = sum' (length list) index list
   | index >= (length list) = sum' take (length list - 1) list
   | otherwise = price (list !! index) + (sum' (take - 1) (index - 1) list)

--Takes in raw list of OrderBookEntry, filters so we only have ENTER entries
--Don't think we need to check for only unique entries (this is n^2, so I'd rather not)
traderEntry :: [OrderBookEntry] -> [OrderBookEntry]
traderEntry list = filter ((== ENTER) . recordType) list

--Take two entries (both bids or both asks) and work out the difference
--The concept of momentum is created from this (as the first entry comes earlier than the second)
--Potentially use time as the Y-coordinate?
difference :: OrderBookEntry -> OrderBookEntry -> Float
difference = undefined

--The function that runs a running tally of the momentum, money available and shares held.
--This function is recursive and returns the result at the end.
traderBrain :: [OrderBookEntry] -> Float -> Float -> [OrderBookEntry] -> ([OrderBookEntry], Float)
traderBrain [] _ money shares = (shares, money)
traderBrain records momentum money shares = do
    let resolution = take 10 records
        remainder = drop 10 records
        newMomentum = momentum resolution
    
