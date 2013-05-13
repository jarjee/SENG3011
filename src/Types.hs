{-# LANGUAGE CPP, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Types (

    -- read/write etc
    getTrades, readF, writeF, doStuff, RecordType(..), recordType,
    enter, trade, amend, delete, cancel, offtr,

    -- types needed for Orderbook
    OrderBookEntry, getId, OrderBook(OrderBook), TransId(transTyp), TradeLog,
    time, price, volume, transId, orders, oldPrice, oldVolume, trans,
	spread, isBid

) where

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Csv
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as HM


--This is used for the printing for the sake of convenience
import Data.Maybe

-- types go here
newtype RecordType = RecordType Int deriving (Show, Eq, Ord)
(amend : cancel : delete : enter : offtr : trade : _) = map RecordType [1..] -- AMEND | CANCEL_TRADE | DELETE | ENTER | OFFTR | TRADE deriving (Show, Read, Eq)
getType :: BL.ByteString -> RecordType
getType "AMEND" = amend
getType "CANCEL_TRADE" = cancel
getType "DELETE" = delete
getType "ENTER" = enter
getType "OFFTR" = offtr
getType "TRADE" = trade
getType x = error $ "Invalid record type:"++show(x)

data OrderBook = OrderBook { orders :: ([OrderBookEntry], [OrderBookEntry]),
                            spread :: Float,
                            priceStep :: Float
                            } deriving (Show, Eq)

-- although this seems pointless at this stage we may need more info later                            
data TradeLog = TradeLog { trades :: ([OrderBookEntry], [OrderBookEntry])
                          } deriving (Show, Eq)                            
                            
data OrderBookEntry = 
        OrderBookEntry { instrument :: String,
                         date :: Integer,
                         time :: String,
                         recordType :: RecordType,
                         price :: Maybe Double,
                         volume :: Maybe Integer,
                         undisclosedVolume :: Maybe Integer,
                         value :: Maybe Float,
                         qualifiers :: String,
                         transId :: Integer,
                         -- bidId :: Maybe Integer, -- When bidAsk is B
                         -- askId :: Maybe Integer, -- When bidAsk is A
                         entryTime :: String,
                         oldPrice :: Maybe Float,
                         oldVolume :: Maybe Integer,
                         -- buyerBrokerId :: Maybe Integer, -- When bidAsk is A
                         -- sellerBrokerId :: Maybe Integer -- When bidAsk is B
                        trans :: TransId
        } deriving (Show, Eq, Ord)

data Trans = Bid { bidId :: Integer, sellerBrokerId :: Maybe Integer }
             | Ask { askId :: Integer, buyerBrokerId :: Maybe Integer } deriving (Show, Read, Eq, Ord)

data TransId = TransId { transTyp :: Char,
                         transContents :: Trans} deriving (Show, Read, Eq, Ord)

getId :: OrderBookEntry -> Integer
getId entry = if (isBid entry) 
                 then bidId $ transContents $ trans entry 
                 else askId $ transContents$ trans entry

makeTrans :: Char -> Maybe Integer -> Maybe Integer -> TransId
makeTrans 'B' (Just x) y = TransId 'B' (Bid x y)
makeTrans 'A' (Just x) y = TransId 'A' (Ask x y)
makeTrans x _ _ = error $ "Cannot match record of type :"++show(x)++" with missing data."

(.:?) :: (FromField a) => NamedRecord -> B.ByteString -> Parser (Maybe a)
obj .:? key = maybe (pure Nothing) (parseField) (HM.lookup key obj)

readF :: String -> IO (Either String (Header, V.Vector OrderBookEntry))
readF name = do
    f <- BL.readFile name
    return $ decodeByName f

writeF :: String -> (Header, V.Vector OrderBookEntry) -> IO()
writeF location (head, elements) = do
	BL.writeFile location (encodeByName head elements)	

db = do
    x <- readF "../test/input.csv"
    either (undefined) (writeF "../test/output.csv") x

doStuff (h, xs) = V.toList xs

instance FromNamedRecord OrderBookEntry where
    parseNamedRecord r = do
    ins <- r.: "#Instrument"
    date <- read <$> r.: "Date"
    time <- r.: "Time"
    reco <- getType <$> r.: "Record Type" 
    pric <- r.:> "Price" 
    volu <- r.:> "Volume" 
    undi <- r.:> "Undisclosed Volume" 
    valu <- r.:> "Value" 
    qual <- r.: "Qualifiers"
    tran <- read <$> r.: "Trans ID" 
    bid <- r .:> "Bid ID" 
    ask <- r.:> "Ask ID" 
    bidAsk <- r.: "Bid/Ask" :: Parser String
    let isb = bidAsk == "B"
    entr <- r.: "Entry Time"
    oldP <- r.:> "Old Price" 
    oldV <- r.:> "Old Volume" 
    buye <- r.:> "Buyer Broker ID" 
    sell <- r.:> "Seller Broker ID"         
    let tranElem = makeTrans (if (null bidAsk) then ' ' else head bidAsk) (if isb then bid else ask) (if isb then sell else buye)

    return $ OrderBookEntry ins date time reco pric volu undi valu qual tran entr oldP oldV tranElem
    where
        obj .:> key = do
            n <- obj .:? key
            return $ n

instance ToNamedRecord OrderBookEntry where
	toNamedRecord (OrderBookEntry inst dat tim recTyp pri vol undisVol val qual trId entryTim oldPri oldVol transElem) = namedRecord $ ["#Instrument" .= inst, "Date" .= show dat, "Time" .= tim, "Record Type" .= show recTyp, "Price" .= showMaybe pri, "Volume" .= showMaybe vol, "Undisclosed Volume" .= showMaybe undisVol, "Value" .= showMaybe val, "Qualifiers" .= qual, "Trans ID" .= show trId] ++ outputTransElem transElem ++ ["Entry Time" .= entryTim, "Old Price" .= showMaybe oldPri, "Old Volume" .= showMaybe oldVol]
outputTransElem tr = bidAskTransElem tr
bidAskTransElem tr = if (isBidTr tr) then (bidTransElem) (transContents tr) else (askTransElem) (transContents tr)
bidTransElem (Bid b sell) = ["Bid ID" .= show b, "Ask ID" .= B.empty, "Bid/Ask" .= 'B', "Buyer Broker ID" .= B.empty, "Seller Broker ID" .= show (maybe ("") (show) sell)]
askTransElem (Ask a buye) = ["Bid ID" .= B.empty, "Ask ID" .= show a, "Bid/Ask" .= 'A', "Buyer Broker ID" .= (maybe ("") (show) buye), "Seller Broker ID" .= B.empty]
emptyTransElem = ["Bid ID" .= B.empty, "Ask ID" .= B.empty, "Bid/Ask" .= B.empty, "Buyer Broker ID" .= B.empty, "Seller Broker ID" .= B.empty]
showMaybe b = maybe "" (show) b

isBidTr :: TransId -> Bool
isBidTr (TransId x _) = x == 'B'

isBid :: OrderBookEntry -> Bool
isBid entry = (transTyp $ trans entry) == 'B'

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
--orderEntry (inst:dat:tim:recTyp:pri:vol:undisVol:val:qual:trId:bId:aId:ba:entryTim:oldPri:oldVol:buyerBrokId:sellerBrokId:[]) = OrderBookEntry inst (read dat) tim (read recTyp) (read pri) (read vol) (read undisVol) (read val) qual (read trId) entryTim (read oldPri) (read oldVol) transElem
--    where
--        isb = ba == "B"
--        transElem = makeTrans (head ba) (if isb then return $ read bId else return $ read aId) (if isb then return $ read sellerBrokId else return $ read buyerBrokId)
orderEntry [] = error "orderEntry cannot take in an empty list! CSV file is not of a valid format!"
orderEntry _ = error "orderEntry must take in exactly the right number of elements! CSV is invalid!"
