import System.IO
import Control.Monad
import Data.List
import Data.String.Utils

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
    let records = map (split ",") $ tail unsplit
    mapM (putStrLn) $ map (foldl (++) "" . intersperse "|") records
    hClose handle

