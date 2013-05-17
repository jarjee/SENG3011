import Types
import Orderbook
import Data.Heap as H
import Test.QuickCheck
--import Data.HashMap as M

main = quickCheck prop_CalcSpread

prop_CalcSpread bidPrice askPrice =
    calculateSpread state == bidPrice - askPrice where
        bstring = "LEGIT,20130517,11:45:11.186,ENTER," ++ show bidPrice ++ ",500,0,300,SHL ,0,,4567898765434567890,B,,,,,203"
        astring = "LEGIT,2013-517,11:45:11.190,ENTER," ++ show askPrice ++ ",500,0,300,SHL ,0,,2345678987654323456,A,,,,,203"
        bidEntry = orderEntry [bstring]
        askEntry = orderEntry [astring]
        bEntryPri = maybe (0) (id) (price bidEntry)
        aEntryPri = maybe (0) (id) (price askEntry)
--        newAMap = M.insert 
        newBHeap = H.insert (bEntryPri, bidEntry) H.empty
        newAHeap = H.insert (aEntryPri, askEntry) H.empty
        state = defaultOrderBookState {buyPrices = newBHeap, sellPrices = newAHeap}
