import Types
import Data.HashMap as M
import Data.Heap as H
import Data.Vector as V

averageRes = 10

data OrderBookState = OrderBookState { entriesNum :: Integer,
                                       average :: Double,
                                       lastSamples :: Vector Double,
                                       buyRecords :: Map Integer OrderBookEntry,
                                       sellRecords :: Map Integer OrderBookEntry,
                                       buyPrices :: MaxPrioHeap Double OrderBookEntry,
                                       sellPrices :: MinPrioHeap Double OrderBookEntry }
                                       deriving (Eq, Show)

defaultOrderBookState = OrderBookState 0 0 V.empty M.empty M.empty H.empty H.empty

updateOrderBook :: OrderBookEntry -> OrderBookState -> OrderBookState
updateOrderBook entry state = do
    let newEntries = (entriesNum state)+1
        result = calculateAverage entry state {entriesNum = newEntries}
    if (recordType entry == enter)
       then enterOrderBook entry result
    else if (recordType entry == delete)
        then deleteOrderBook entry result
    else error "Hello!"
   
enterOrderBook :: OrderBookEntry -> OrderBookState -> OrderBookState
enterOrderBook entry state = do
    let idNum = getId entry
        entryPri = maybe (0) (id) (price entry)
    --Currently assumes everything is an enter, so only entering data.
    if (isBid entry) then do --Do data processing for Bids
        let newMap = M.insert idNum entry $ buyRecords state
            newHeap = H.insert (entryPri, entry) $ buyPrices state
        state {buyRecords = newMap, buyPrices = newHeap}
    else do --Do data processing for Asks
        let newMap = M.insert idNum entry $ sellRecords state
            newHeap = H.insert (entryPri, entry) $ sellPrices state
        state {sellRecords = newMap, sellPrices = newHeap}

deleteOrderBook :: OrderBookEntry -> OrderBookState -> OrderBookState
deleteOrderBook entry state = state

calculateAverage :: OrderBookEntry -> OrderBookState -> OrderBookState
calculateAverage entry state = do
    let lenVal = V.length $ lastSamples state
        val = maybe 0 id (price $ entry)
        removeVal = V.last $ lastSamples state
        newVector = V.cons val $ if (lenVal >= averageRes) then V.init $ lastSamples state else lastSamples state
        divisor = fromIntegral averageRes
        lenDiv = fromIntegral lenVal
        result = state {lastSamples = newVector}
    if (lenVal < averageRes) then do
        let newAverage = ((average state) * (lenDiv - 1) + val)/if (lenDiv > 0) then lenDiv else 1
        result {average = newAverage}
    else do
        let newAverage = (average state) - (removeVal/divisor) + (val/divisor)
        result {average = newAverage}
