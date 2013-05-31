module MakeData where
import System.Random
import System.IO.Unsafe
import Text.JSON
import System.IO
import Control.Monad.Trans.Writer.Strict
import Control.Proxy


{-# NOINLINE myRandom #-}

data Evaluation = 
        Evaluation {startMon :: Double,
                    endMon :: Double,
                    monMade :: Double,
                    numBought :: Integer,
                    numSold :: Integer,
                    numHolding :: Integer,
                    peaks :: [(String, Double)], --(time, price)
                    valleys :: [(String, Double)], --(time, price)
                    bids :: [(String, Double)],
                    asks :: [(String, Double)],
                    bidAccuracy :: Double,
                    askAccuracy :: Double
                    } deriving (Show, Eq)

convertEval :: Evaluation -> JSValue
convertEval (Evaluation startMon endMon monMade numBought numSold numHolding peaks valleys bids asks bidAccuracy askAccuracy) = makeObj $ [("stats", evalStats startMon endMon monMade numBought numSold numHolding),("peaks", showJSON $ convertTuples peaks),("valleys", showJSON $ convertTuples valleys),("bids", showJSON $ convertTuples bids),("asks", showJSON $ convertTuples asks),("bid_accuracy", showJSON bidAccuracy),("ask_accuracy", showJSON askAccuracy)]

convertTuples :: (JSON a, JSON b) => [(a,b)] -> [JSValue]
convertTuples l = map (makeObj . (\(x,y) -> [("fst",showJSON x),("snd",showJSON y)])) l

evalStats startMon endMon monMade numBought numSold numHolding = makeObj $ [("start_money", showJSON startMon),("end_money", showJSON endMon),("profit", showJSON monMade),("number_bought", showJSON numBought),("number_sold", showJSON numSold), ("number_held", showJSON numHolding)]

main = do
    let startMoney = 10000
        endMoney = 10500
        profit = 500
        sharesBought = 3000
        sharesSold = 3000
        sharesHolding = 0
        -- Next few are type [(String, Double)]
        randomReal = makeRandomReal 0
        randomPeaks = sortPeaks randomReal
        randomValleys = sortValleys randomReal
        randomBids = makeRandomBids 0
        randomAsks = makeRandomAsks 0
        peakTimes = getTimes randomPeaks
        valleyTimes = getTimes randomValleys
        bidTimes = getTimes randomBids
        askTimes = getTimes randomAsks
        bidAccuracy = calcAccuracy valleyTimes bidTimes
        askAccuracy = calcAccuracy peakTimes askTimes
        eval = Evaluation startMoney endMoney profit sharesBought sharesSold sharesHolding randomPeaks randomValleys randomBids randomAsks bidAccuracy askAccuracy
        evalJSON = encode $ convertEval eval
    writeFile "RandomJSON.txt" evalJSON


-- timeString (10-14):(00-59):(00-59):(000-999)
-- price (10-50)
makeRandomReal :: Integer -> [(String, Double)]
makeRandomReal depth
    | depth < 10000 = [(randomTimeString, randomPrice)] ++ makeRandomReal (depth + 1)
    | otherwise = []

--            let x = unsafePerformIO $ randomRIO (0.0, 1.0)

-- (10-14):(00-59):(00-59):(000-999)
randomTimeString :: String
randomTimeString = timeString
    where hours = floor $ myRandom 10 14
          minutes = floor $ myRandom 0 59
          seconds = floor $ myRandom 0 59
          milSeconds = floor $ myRandom 0 999
          timeString = (show hours) ++ ":" ++ (show minutes) ++ ":" ++ (show seconds) ++ "." ++ (show milSeconds)

randomPrice :: Double
randomPrice l h = myRandom l h

myRandom :: Double -> Double -> IO Double
myRandom x y = randomRIO (x,y)

sortPeaks :: [(String,Double)] -> [(String,Double)]
sortPeaks [] = []
sortPeaks [x] = []
sortPeaks [x,y]
    | price2 > price1 = [(time2,price2)]
    | otherwise = []
    where price1 = snd x
          price2 = snd y
          time2 = fst y
sortPeaks [x,y,z]
    | price2 > price1 && price2 > price3 = [(time2,price2)]
    | price3 > price2 = [(time3,price3)]
    | otherwise = []
    where price1 = snd x
          price2 = snd y
          price3 = snd z
          time2 = fst y
          time3 = fst z
sortPeaks (x:y:z:rest)
    | price2 > price1 && price2 > price3 = [(time2,price2)] ++ sortPeaks (z:rest)
    | otherwise = sortPeaks (y:z:rest)
    where price1 = snd x
          price2 = snd y
          price3 = snd z
          time2 = fst y

sortValleys :: [(String,Double)] -> [(String,Double)]
sortValleys [] = []
sortValleys [x] = []
sortValleys [x,y]
    | price2 < price1 = [(time2,price2)]
    | otherwise = []
    where price1 = snd x
          price2 = snd y
          time2 = fst y
sortValleys [x,y,z]
    | price2 < price1 && price2 < price3 = [(time2,price2)]
    | price3 < price2 = [(time3,price3)]
    | otherwise = []
    where price1 = snd x
          price2 = snd y
          price3 = snd z
          time2 = fst y
          time3 = fst z
sortValleys (x:y:z:rest)
    | price2 < price1 && price2 < price3 = [(time2,price2)] ++ sortValleys (z:rest)
    | otherwise = sortValleys (y:z:rest)
    where price1 = snd x
          price2 = snd y
          price3 = snd z
          time2 = fst y
          time3 = fst z

makeRandomBids :: Integer -> [(String,Double)]
makeRandomBids depth
    | depth < 3000 = [(randomTimeString,randomPrice)] ++ makeRandomBids (depth + 1)
    | otherwise = []

makeRandomAsks :: Integer -> [(String, Double)]
makeRandomAsks depth
    | depth < 3000 = [(randomTimeString,randomPrice)] ++ makeRandomAsks (depth + 1)
    | otherwise = []

getTimes :: [(String, Double)] -> [Double]
getTimes [] = []
getTimes [(timeString,_)] = [formatTime $ makeList timeString]
getTimes ((timeString,_):rest) = [formatTime $ makeList timeString] ++ getTimes rest

formatTime :: [String] -> Double
formatTime (hours:minutes:seconds:milliseconds:[]) = do
    let numHour = (read hours) * 3600
        numMinute = (read minutes) * 60
        numSecond = read seconds
        numMilliseconds = (read milliseconds) / 1000
        actualTime = numHour + numMinute + numSecond + numMilliseconds
    actualTime
formatTime _ = 0

-- taken from http://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell which is a modified version of prelude's function words
splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s = case dropWhile p s of 
                     "" -> []
                     s' -> w : splitWhen p s''
                           where (w, s'') = break p s'

-- THIS IS ONLY FOR ONE USE AND OTHERWISE IS SILLY
makeList :: String -> [String]
makeList timeString = newList
    where firstList = splitWhen (==':') timeString
          secondList = splitWhen (=='.') $ last $ firstList
          newList = (take 2 $ firstList) ++ secondList

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
        accuracy = (tempAccuracy + (calcAccuracy xs ys * (fromIntegral $ length ys))) / ((fromIntegral $ length ys) + 1)
    accuracy

findClosest :: Double -> [Double] -> Double
findClosest item list = item

measureAccuracy :: Double -> Double
measureAccuracy diff
    | diff < 0 = measureAccuracy (diff * (-1))
    | otherwise = 100 - diff -- not necessarily the best measure, can change later
