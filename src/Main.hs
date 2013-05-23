import System.Environment
import System.IO
import Control.Monad
import Data.List
import qualified Data.Vector as V
import Data.Csv

import Types
import Orderbook
import Trader
import Views

data MainInput = MainInput { inputCash :: Float,
                             buyAlg :: String,
                             sellAlg :: String} deriving (Show, Read)

main = do
    args <- getArgs
    if (length args == 4) then process args else putStrLn "Please give FileName Money BuyAlgorithm SellAlgorithm"

process :: [String] -> IO()
process arg = do
    file <- readF $ head arg
    parse (MainInput (read $ arg !! 1) (arg !! 2) (arg !! 3)) file

parse :: MainInput -> Either String (Header, V.Vector OrderBookEntry) -> IO()
parse input fields = either (\x -> putStrLn "Failed to parse the provided file.\nPlease check for corruption.") (undefined) fields --(dataProcessing input) fields

{-
    This is the preliminary recursive function that we shall use for generating our strategies.
    Currently this can parse vectors correctly, but would be slow for very long strings (Due to no pattern matching).
    It'd be possible to improve this, but since this only gets called at the start (and never afterwards)
    I don't feel it's worth the effort to improve.
-}
functionParse :: V.Vector String -> Int -> (Int, (TraderState -> TraderState))
functionParse list loc
    | loc >= V.length list = error "Error in parsing"
    | loc < 0 = error "Error in parsing"
    | otherwise = do
        if list V.! loc == "gradient" then do
            let peak = functionParse list $ loc+1
                valley = functionParse list (fst peak)
                neither = functionParse list (fst valley)
            (fst neither ,gradientSwitch (snd peak) (snd valley) (snd neither))
        else if list V.! loc == "nothing" then (loc+1, nothing)
        else
            (loc+1, nothing)

--dataProcessing :: MainInput -> (Header, V.Vector OrderBookEntry) -> IO()
--dataProcessing input (head, fields) = do
--    let allRecords = V.toList fields
--        cash = inputCash input
--        tradeRecords = traderEntry $ allRecords
--        tradeResult = traderBrain tradeRecords $ defaultTraderState {money = cash}
--    putStrLn $ "The Trader was given : $"++show(cash)
--    putStrLn $ "The Trader ended up with:"
--    putStrLn $ '$':show(money tradeResult)
--    putStrLn $ "Holding "++show(length $ sha tradeResult)++" shares."-- ++show((sum $ map (\x -> (shaPri x) * fromInteger (shAmt x)) $ sha tradeResult))
--    putStrLn $ "And had sold "++show(length $ his tradeResult)++" shares."-- ++show((sum $ map (\x -> (shaPri x) * fromInteger (shAmt x)) $ his tradeResult))

orderBookLoop :: [OrderBookEntry] -> OrderBookState -> OrderBookState
orderBookLoop [] state = state
orderBookLoop (record:rest) state = orderBookLoop rest $ updateOrderBook record state
