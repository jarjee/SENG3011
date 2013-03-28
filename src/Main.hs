import System.IO
import Control.Monad
import Data.List

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

main = do
    handle <- openFile "../test/test.csv" ReadMode
    content <- hGetContents handle
    let unsplit = lines content
    let records = map (split (== ',')) $ unsplit
    mapM (putStrLn) $ map (foldl (++) "" . intersperse "|") records
    hClose handle

split :: (a -> Bool) -> [a] -> [[a]]
split pr [] = []
split pr l = h : (split pr f)
    where
        (h,t) = break pr l
        f = if null t then [] else tail t

dropPred :: (a -> Bool) -> [a] -> [a]
dropPred _ [] = []
dropPred pr (x:xs) = if pr x then xs else x:dropPred pr xs
