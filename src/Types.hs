module Types (getTrades, csvRep) where
-- types go here
data OrderBookEntry = 
        OrderBookEntry { instrument :: String,
                         date :: Integer,
                         time :: String,
                         recordType :: RecordType,
                         price :: Float,
                         volume :: Integer,
                         undisclosedVolume :: Integer,
                         value :: Float,
                         qualifiers :: String,
                         transId :: Integer,
                         bidId :: Integer,
                         askId :: Integer,
			 bidAsk :: String,
                         entryTime :: String,
                         oldPrice :: Float,
                         oldVolume :: Integer,
                         buyerBrokerId :: Integer,
                         sellerBrokerId :: Integer
        }

data RecordType = AMEND | CANCEL_TRADE | DELETE | ENTER | OFFTR | TRADE deriving (Show, Read, Eq)

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
orderEntry (inst:dat:tim:recTyp:pri:vol:undisVol:val:qual:trId:bId:aId:ba:entryTim:oldPri:oldVol:buyerBrokId:sellerBrokId:[]) = OrderBookEntry inst (read dat) tim (read recTyp) (read pri) (read vol) (read undisVol) (read val) qual (read trId) (read bId) (read aId) ba entryTim (read oldPri) (read oldVol) (read buyerBrokId) (read sellerBrokId)
orderEntry [] = error "orderEntry cannot take in an empty list! CSV file is not of a valid format!"
orderEntry _ = error "orderEntry must take in exactly the right number of elements! CSV is invalid!"

--Final Output
csvRep :: [OrderBookEntry] -> String
csvRep orders = undefined
