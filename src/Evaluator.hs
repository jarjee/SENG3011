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

data Evaluation = 
        Evaluation {startMon :: Double,
                    endMon :: Double,
                    monMade :: Double,
                    numBought :: Integer,
                    numSold :: Integer,
                    numHolding :: Integer,
                    peaks :: [(Double, Double)], --(time, price)
                    valleys :: [(Double, Double)], --(time, price)
                    bids :: [(Double, Double)],
                    asks :: [(Double, Double)],
                    bidAccuracy :: Double,
                    askAccuracy :: Double
                    } deriving (Show, Eq)

-- takes in startMoney, endMoney, orderBook, listSharesBought/Sold. Returns a string?
compileEvalInfo :: Double -> Double -> [OrderBookEntry] -> [OrderBookEntry] -> String
compileEvalInfo startMon endMon oBook shareList = do
    let monMade = endMon - startMon
        numBought = countBid shareList
        numSold = countAsk shareList
        numHolding = numBought - numSold -- assuming that we don't start with any shares?
        peaks = getPeaks oBook
        valleys = getValleys oBook
        bids = getBids shareList
        asks = getAsks shareList
        peakTimes = getTimes peaks
        valleyTimes = getTimes valleys
        bidTimes = getTimes bids
        askTimes = getTimes asks
        bidAccuracy = calcAccuracy valleyTimes bidTimes
        askAccuracy = calcAccuracy peakTimes askTimes
--        returnString = (show startMon) ++ " " ++ (show endMon) ++ " " ++ (show monMade) ++ " " ++ (show numBought) ++ " " ++ (show numSold) ++ " " ++ (show numHolding) ++ " " ++ (show bidAccuracy) ++ " " ++ (show askAccuracy)
    Evaluation startMon endMon monMade numBought numSold numHolding peaks valleys bids asks bidAccuracy askAccuracy

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

getPeaks :: [OrderBookEntry] -> [(String,Double)]
getPeaks [] = []
getPeaks [x] = []
getPeaks [x,y]
    | price2 > price1 = [(time2,price2)]
    | otherwise = []
    where price1 = maybe (0) (id) $ price x
          price2 = maybe (0) (id) $ price y
          time2 = time y
getPeaks [x,y,z]
    | price2 > price1 && price2 > price3 = [(time2,price2)]
    | price3 > price2 = [(time3,price3)]
    | otherwise = []
    where price1 = maybe (0) (id) $ price x
          price2 = maybe (0) (id) $ price y
          price3 = maybe (0) (id) $ price z
          time2 = time y
          time3 = time z
getPeaks (x:y:z:rest)
    | price2 > price1 && price2 > price3 = (time2,price2) ++ getPeaks (z:rest)
    | otherwise = getPeaks (y:z:rest)
    where price1 = maybe (0) (id) $ price x
          price2 = maybe (0) (id) $ price y
          price3 = maybe (0) (id) $ price z
          time2 = time y

getValleys :: [OrderBookEntry] -> [(String,Double)]
getValleys [] = []
getValleys [x] = []
getValleys [x,y]
    | price2 < price1 = [(time2,price2)]
    | otherwise = []
    where price1 = maybe (0) (id) $ price x
          price2 = maybe (0) (id) $ price y
          time2 = time y
getValleys [x,y,z]
    | price2 < price1 && price2 < price3 = [(time2,price2)]
    | price3 < price2 = [(time3,price3)]
    | otherwise = []
    where price1 = maybe (0) (id) $ price x
          price2 = maybe (0) (id) $ price y
          price3 = maybe (0) (id) $ price z
          time2 = time y
          time3 = time z
getValleys (x:y:z:rest)
    | price2 < price1 && price2 < price3 = (time2,price2) ++ getValleys (z:rest)
    | otherwise = getValleys (y:z:rest)
    where price1 = maybe (0) (id) $ price x
          price2 = maybe (0) (id) $ price y
          price3 = maybe (0) (id) $ price z
          time2 = time y

getBids :: [OrderBookEntry] -> [(String,Double)]
getBids [] = []
getBids [x]
    | isBid x = [(time x, maybe (0) (id) $ price x)]
    | otherwise = []
getBids (x:xs)
    | isBid x = (time x, maybe (0) (id) $ price x) ++ getBids xs
    | otherwise = getBids xs

getAsks :: [OrderBookEntry] -> [(String,Double)]
getAsks [] = []
getAsks [x]
    | isBid x = []
    | otherwise = [(time x, maybe (0) (id) $ price x)]
getAsks (x:xs)
    | isBid x = getAsks xs
    | otherwise = (time x, maybe (0) (id) $ price x) ++ getAsks xs

getTimes :: [(String, Double)] -> [Double]
getTimes [] = []
getTimes [(timeString,_)] = [formatTime timeString]
getTimes ((timeString,_):rest) = formatTime timeString ++ getTimes rest

formatTime :: String -> Double
formatTime [hours,":",minutes,":",seconds,".",milliseconds] = do
    let numHour = (read hours) * 3600
        numMinute = (read minutes) * 60
        numSecond = read seconds
        numMilliseconds = (read milliseconds) / 1000
        actualTime = numHour + numMinute + numSecond + numMilliseconds
    actualTime
formatTime _ = 0

-- first is list of actual peaks/valleys, and second is list of bid/sell times
-- might need to be done as functions specific for if checking bid accuracy versus ask accuracy
calcAccuracy :: [Double] -> [Double] -> Double
calcAccuracy [] [] = 100
calcAccuracy [] _ = 0
calcAccuracy _ [] = 0
calcAccuracy xs [y] = do
    let closest = findClosest y xs
        diff = closest - y
        accuracy = measureAccuracy diff
    accuracy
calcAccuracy xs (y:ys) = do
    let closest = findClosest y xs
        diff = closest - y
        tempAccuracy = measureAccuracy diff
        accuracy = (tempAccuracy + (calcAccuracy xs ys * length ys)) / (length ys + 1)
    accuracy

measureAccuracy :: Double -> Double
measureAccuracy diff
    | diff < 0 = measureAccuracy (diff * -1)
    | otherwise = 100 - diff -- not necessarily the best measure, can change later
