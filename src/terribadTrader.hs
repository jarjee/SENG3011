--Takes in raw list of OrderBookEntry, filters so we only have ENTER entries
--Don't think we need to check for only unique entries (this is n^2, so I'd rather not)
traderEntry :: [OrderBookEntry] -> [OrderBookEntry]
traderEntry list = filter ((== ENTER) . recordType) list

--Take two entries (both bids or both asks) and work out the difference
--The concept of momentum is created from this (as the first entry comes earlier than the second)
--Potentially use time as the Y-coordinate?
difference :: OrderBookEntry -> OrderBookEntry -> Float

--The function that runs a running tally of the momentum, money available and shares held.
--This function is recursive and returns the result at the end.
traderBrain :: [OrderBookEntry] -> (Float, Float) -> Float -> [OrderBookEntry] -> [OrderBookEntry]
traderBrain records (bidMomentum, askMomentum) money shares = 
