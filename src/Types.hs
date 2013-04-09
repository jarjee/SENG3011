{-# LANGUAGE CPP, OverloadedStrings #-}

module Types (getTrades, csvRep) where
-- THIS REQUIRES CASSAVA FOR ALL THE SPECIAL FUNCTIONALITY, BY DEFAULT IT IS NOT AN INTELLIGENT PARSER.

--BUILD ALL THE CASS WITH -DCASS
#ifdef CASS

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Csv
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as HM

#endif

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
                         bidId :: Maybe Integer,
                         askId :: Maybe Integer,
			 bidAsk :: String,
                         entryTime :: String,
                         oldPrice :: Maybe Float,
                         oldVolume :: Integer,
                         buyerBrokerId :: Integer,
                         sellerBrokerId :: Integer
        } deriving (Show, Read, Eq)

#ifdef CASS
(.:?) :: (FromField a) => NamedRecord -> B.ByteString -> Parser (Maybe a)
obj .:? key = case HM.lookup key obj of
		Nothing -> pure Nothing
		
		Just v -> do
			if (not $ B.null v) then 
				Just <$> parseField (v)
			else
				pure Nothing

-- maybe (fail "DICKS") (undefined) (maybe Nothing (parseField) $ HM.lookup key obj) 
instance FromNamedRecord OrderBookEntry where
    parseNamedRecord r = do
	ins <- r.: "#Instrument"
	date <- read <$> r.: "Date"
	time <- r.: "Time"
	reco <- read <$> r.: "Record Type" 
	pric <- read <$> r.: "Price" 
	volu <- read <$> r.: "Volume" 
	undi <- read <$> r.: "Undisclosed Volume" 
	valu <- read <$> r.: "Value" 
	qual <- r.: "Qualifiers"
	tran <- read <$> r.: "Trans ID" 
	bid <- r .:> "Bid ID" 
	ask <- r.:> "Ask ID" 
	bidAsk <- r.: "Bid/Ask"
	entr <- r.: "Entry Time"
	oldP <- r.:> "Old Price" 
	oldV <- read <$> r.: "Old Volume" 
	buye <- read <$> r.: "Buyer Broker ID" 
	sell <- read <$> r.: "Seller Broker ID" 		

	return $ OrderBookEntry ins date time reco pric volu undi valu qual tran bid ask bidAsk entr oldP oldV buye sell
	where
		obj .:> key = do
			n <- obj .:? key
			return $ read <$> n

#else

#endif

-- maybe BidList, AskList and OrderBook types...
data OrderBook = 
	OrderBook 	{ 	askList :: [OrderBookEntry],
				bidList :: [OrderBookEntry],
				spread :: Float
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
