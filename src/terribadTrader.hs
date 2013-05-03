momentum :: [OrderBookEntry] -> Integer -> numShares -> money -> ([OrderBookEntry], numShares, money)

--Takes in raw list of OrderBookEntry, filters so we only have ENTER entries
traderEntry :: [OrderBookEntry] -> [OrderBookEntry]

--Having a recursive tally of momentum, 
