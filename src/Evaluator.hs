--NOTE: This is currently a semi-rough model of how this might work, as I'm not sure where the code would be put in the overall program, 
--      and thus it may need adjustments in architecture/method depending on where it ends up and how wrong it may end up being
--      also subject to any adjustments for optimisations in future

{-
things for evaluator:
money started with
money ended with
money made (derived)
the shares bought throughout (orderBookEntries for bids/asks)
numShares bought
numShares sold
numShares holding
graph of peaks/valleys, showing when shares were bought -- graph may have to be done on gui side?
average accuracy %? (based on how close to a literal peak/valley shares were sold/bought)
-}

-- takes in startMoney, endMoney, orderBook, listSharesBought/Sold. Returns a string?
compileEvalInfo :: Double -> Double -> [OrderBookEntry] -> [OrderBookEntry] -> String
compileEvalInfo startMon endMon oBook shareList = do
    let monMade = endMon - startMon
        numBought = countBid shareList
        numSold = countAsk shareList
        numHolding = numBought - numSold -- assuming that we don't start with any shares?
        peakTimes = getPeaks oBook
        valleyTimes = getValleys oBook
        bidTimes = getBids shareList
        askTimes = getAsks shareList
        bidAccuracy = calcAccuracy valleyTimes bidTimes
        askAccuracy = calcAccuracy peakTimes askTimes
        returnString = (show startMon) ++ " " ++ (show endMon) ++ " " ++ (show monMade) ++ " " ++ (show numBought) ++ " " ++ (show numSold) ++ " " ++ (show numHolding) ++ " " ++ (show bidAccuracy) ++ " " ++ (show askAccuracy)
    returnString

countBid :: [OrderBookEntry] -> Integer
countBid [] = 0
countBid [x]
    | isBid x = 1
    | otherwise = 0
countBid (x:xs)
    | isBid x = 1 + countBid xs
    | otherwise = countBid xs

countAsk :: [OrderBookEntry] -> Integer
countAsk [] = 0
countAsk [x]
    | isBid x = 0
    | otherwise = 1
countAsk (x:xs)
    | isBid x = countAsk xs
    | otherwise = 1 + countAsk xs

getPeaks :: [OrderBookEntry] -> [Double]

getValleys :: [OrderBookEntry] -> [Double]

-- first is list of actual peaks/valleys, and second is list of bid/sell times
-- might need to be done as functions specific for if checking bid accuracy versus ask accuracy
calcAccuracy :: [Double] -> [Double] -> Double

