{-# LANGUAGE CPP, OverloadedStrings #-}
module Types (

    -- read/write etc
    getTrades, db,

    -- types needed for Orderbook
    OrderBookEntry, OrderBook(OrderBook), TransId(Bid,Ask), TradeLog,
    time, price, volume, transId, orders, oldPrice, oldVolume, trans,
	spread

) where

-- THIS REQUIRES CASSAVA FOR ALL THE SPECIAL FUNCTIONALITY, BY DEFAULT IT IS NOT AN INTELLIGENT PARSER.

--BUILD ALL THE CASS WITH -DCASS

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Csv
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as HM


--This is used for the printing for the sake of convenience
import Data.Maybe

-- types go here
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
                         price :: Maybe Float,
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
            trans :: Maybe TransId
        } deriving (Show, Read, Eq)

data TransId =     Bid { bidId :: Integer, sellerBrokerId :: Integer }
                | Ask { askId :: Integer, buyerBrokerId :: Integer } deriving (Show, Read, Eq)

makeTrans :: Bool -> Maybe Integer -> Maybe Integer -> Maybe TransId
makeTrans True (Just x) (Just y) = Just $ Bid x y
makeTrans False (Just x) (Just y) = Just $ Ask x y
makeTrans _ _ _ = Nothing

(.:?) :: (FromField a) => NamedRecord -> B.ByteString -> Parser (Maybe a)
obj .:? key = case HM.lookup key obj of
        Nothing -> pure Nothing
        
        Just v -> do
            if (not $ B.null v) then 
                Just <$> parseField (v)
            else
                pure Nothing

readF :: IO (Either String (Header, V.Vector OrderBookEntry))
readF = do
    f <- BL.readFile "../test/test.csv"
    return $ decodeByName f

writeF :: String -> (Header, V.Vector OrderBookEntry) -> IO()
writeF location (head, elements) = do
	BL.writeFile location (encodeByName head elements)	

db = do
    x <- readF
    either (undefined) (writeF "../test/output.csv") x

doStuff (h, xs) = return $ V.toList xs

instance FromNamedRecord OrderBookEntry where
    parseNamedRecord r = do
    ins <- r.: "#Instrument"
    date <- read <$> r.: "Date"
    time <- r.: "Time"
    reco <- read <$> r.: "Record Type" 
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
    let tranElem = makeTrans isb (if isb then bid else ask) (if isb then sell else buye)

    return $ OrderBookEntry ins date time reco pric volu undi valu qual tran entr oldP oldV tranElem
    where
        obj .:> key = do
            n <- obj .:? key
            return $ read <$> n

instance ToNamedRecord OrderBookEntry where
	toNamedRecord (OrderBookEntry inst dat tim recTyp pri vol undisVol val qual trId entryTim oldPri oldVol transElem) = namedRecord $ ["#Instrument" .= inst, "Date" .= show dat, "Time" .= tim, "Record Type" .= show recTyp, "Price" .= showMaybe pri, "Volume" .= showMaybe vol, "Undisclosed Volume" .= showMaybe undisVol, "Value" .= showMaybe val, "Qualifiers" .= qual, "Trans ID" .= show trId] ++ outputTransElem transElem ++ ["Entry Time" .= show entryTim, "Old Price" .= showMaybe oldPri, "Old Volume" .= showMaybe oldVol]
outputTransElem tr = ["Bid ID" .= show "", "Ask ID" .= show "", "Bid/Ask" .= show "", "Buyer Broker ID" .= show "", "Seller Broker ID" .= show ""]
showMaybe b = maybe "" (show) b
isBid (Bid _ _) = True
isBid (Ask _ _) = False

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
orderEntry (inst:dat:tim:recTyp:pri:vol:undisVol:val:qual:trId:bId:aId:ba:entryTim:oldPri:oldVol:buyerBrokId:sellerBrokId:[]) = OrderBookEntry inst (read dat) tim (read recTyp) (read pri) (read vol) (read undisVol) (read val) qual (read trId) entryTim (read oldPri) (read oldVol) transElem
    where
        isb = ba == "B"
        transElem = makeTrans isb (if isb then return $ read bId else return $ read aId) (if isb then return $ read sellerBrokId else return $ read buyerBrokId)
orderEntry [] = error "orderEntry cannot take in an empty list! CSV file is not of a valid format!"
orderEntry _ = error "orderEntry must take in exactly the right number of elements! CSV is invalid!"
