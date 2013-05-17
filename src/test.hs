import Types
import Orderbook
import Trader
import Data.Heap as H
import Test.QuickCheck
import Data.HashMap as M

main = quickCheck prop_CalcSpread

makeTestBid :: Double -> OrderBookEntry
makeTestBid bidPrice = bidEntry where
    bString = ["LGT","20130517","11:45:11.186","ENTER",(show bidPrice),"500","0","21083.5","SHL","0","","4567898765434567890","B","","","","","203"]
    bidEntry = orderEntry bString

makeTestAsk :: Double -> OrderBookEntry
makeTestAsk askPrice = askEntry where
    astring = ["LGT","20130517","11:45:11.190","ENTER",(show askPrice),"500","0","21083.5","SHL","0","","2345678987654323456","A","","","","","203"]
    askEntry = orderEntry astring

makeTestTrade :: Double -> OrderBookEntry -> OrderBookEntry -> OrderBookEntry
makeTestTrade price bidEntry askEntry = tradeEntry where
    bId = getId bidEntry
    aId = getId askEntry
    tString = ["LGT","20130517","11:45:12.000","TRADE",(show price),"500","0","21083.5","SHL","0",(show bId),(show aId),"","","","","210","203"]
    tradeEntry = orderEntry tString
    

makeTestState :: OrderBookEntry -> OrderBookEntry -> OrderBookState
makeTestState bidEntry askEntry = state where
    bId = getId bidEntry
    aId = getId askEntry
    newBMap = M.insert bId bidEntry M.empty
    newAMap = M.insert aId askEntry M.empty
    bEntryPri = maybe (0) (id) (price bidEntry)
    aEntryPri = maybe (0) (id) (price askEntry)
    newBHeap = H.insert (bEntryPri, bidEntry) H.empty
    newAHeap = H.insert (aEntryPri, askEntry) H.empty
    state = defaultOrderBookState {buyRecords = newBMap, sellRecords = newAMap, buyPrices = newBHeap, sellPrices = newAHeap}

prop_CalcSpread bidPrice askPrice =
    calculateSpread state == bidPrice - askPrice where
        bidEntry = makeTestBid bidPrice
        askEntry = makeTestAsk askPrice
        state = makeTestState bidEntry askEntry

prop_tradeOrderBook price =
    tradeOrderBook tradeEntry state == defaultOrderBookState where
        bidEntry = makeTestBid price
        askEntry = makeTestAsk price
        tradeEntry = makeTestTrade price bidEntry askEntry
        state = makeTestState bidEntry askEntry

prop_gradientTest first last = 
    sum vp == difference where
        difference = floor $ abs (first - last)
        gradientTest l = map (test l) [3..length(l)]
        test l x = gradientSwitch (reverse $ Prelude.take x l) (defaultTraderState) (found) (found) (neither)
        found s = 1
        neither s = 0
        listVals f s = map ((+70) . (*10) . sin . (pi*)) [f,(f+0.025)..s] 
        vp = gradientTest $ listVals first last
