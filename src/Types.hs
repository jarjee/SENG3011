import System.IO
-- types go here
data OrderBookEntry = 
        OrderBookEntry { instrument :: String,
                         date :: String,
                         time :: String,
                         recordType :: String,
                         price :: String,
                         volume :: String,
                         undisclosedVolume :: String,
                         value :: String,
                         qualifiers :: String,
                         transId :: String,
                         bidId :: String,
                         askId :: String,
                         entryTime :: String,
                         oldPrice :: String,
                         oldVolume :: String,
                         buyerBrokerId :: String,
                         sellerBrokerId :: String
        }


split :: (a -> Bool) -> [a] -> [[a]] 
split pr [] = []
split pr l = h : (split pr f)
    where
        (h,t) = break pr l
        f = if null t then [] else tail t

dropPred :: (a -> Bool) -> [a] -> [a]
dropPred _ [] = []
dropPred pr (x:xs) = if pr x then xs else x:dropPred pr xs

getTrades :: String -> [OrderBookEntry]
getTrades handle = do
	let records = lines handle
	map (orderEntry . split (==',')) records

orderEntry :: [String] -> OrderBookEntry
orderEntry (inst:dat:tim:recTyp:pri:vol:undisVol:val:qual:trId:bId:aId:entryTim:oldPri:oldVol:buyerBrokId:sellerBrokId:[]) = OrderBookEntry inst dat tim recTyp pri vol undisVol val qual trId bId aId entryTim oldPri oldVol buyerBrokId sellerBrokId
orderEntry [] = undefined
orderEntry _ = undefined
