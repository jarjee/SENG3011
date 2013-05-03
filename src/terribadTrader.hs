import Types

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
    
