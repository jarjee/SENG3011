import Types as T
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
updateOrderBook entry state
    | (recordType entry) == T.enter = enterOrderBook entry result
    | (recordType entry) == T.delete = deleteOrderBook entry result
    | otherwise = error "Not valid type"
    where
        newEntries = (entriesNum state)+1
        result = calculateAverage entry state {entriesNum = newEntries}

enterOrderBook :: OrderBookEntry -> OrderBookState -> OrderBookState
enterOrderBook entry state = do
    let idNum = getId entry
        entryPri = maybe (0) (id) (price entry)
    if (isBid entry) then do --Do data processing for Bids
        let newMap = M.insert idNum entry $ buyRecords state
            newHeap = H.insert (entryPri, entry) $ buyPrices state
        state {buyRecords = newMap, buyPrices = newHeap}
    else do --Do data processing for Asks
        let newMap = M.insert idNum entry $ sellRecords state
            newHeap = H.insert (entryPri, entry) $ sellPrices state
        state {sellRecords = newMap, sellPrices = newHeap}

deleteOrderBook :: OrderBookEntry -> OrderBookState -> OrderBookState
deleteOrderBook entry state = do
    let idNum = getId entry
    if (isBid entry) then do
        let newMap = M.delete (getId entry) (buyRecords state)
            newHeap = H.fromList $ Prelude.map (\(x,y) -> (maybe (0) (id) (price y),y)) $ M.toList newMap
        state {buyRecords = newMap, buyPrices = newHeap}
    else do
        let newMap = M.delete (getId entry) (sellRecords state)
            newHeap = H.fromList $ Prelude.map (\(x,y) -> (maybe (0) (id) (price y),y)) $ M.toList newMap
        state {sellRecords = newMap, sellPrices = newHeap}

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
