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

data MainInput = MainInput { inputCash :: Double,
                             algorithm :: (TraderState -> TraderState)}

main = do
    args <- getArgs
    if (length args == 3) then process args else putStrLn "Please give FileName Money Algorithm"

process :: [String] -> IO()
process arg = do
    if "help" `elem` arg then helpMsg else do
        file <- readF $ head arg
        parse (MainInput (read $ arg !! 1) (createStrategy $ arg !! 2)) file

helpMsg :: IO()
helpMsg = do
    progName <- getProgName
    putStrLn $ concat ["Welcome to Team 6's High Frequency Trading simulator!\n",
           "Here's a run down of how to use this piece of software.\n",
           "Please call the program with all the information that is required for it to run.\n",

           progName, " name-of-file amount-of-money trading-algorithm\n",
           "Where name-of-file is the file that is going to be parsed,",
           "amount-of-money is the amount of money that you want to give the trader initially.\n",

           "The algorithm is slightly more complex as we let you build your custom strategies from ",
           "building blocks that we provide. The algorithms are broken into 'Deciders' and 'Actors'.\n",
           "-----------------\n",
           "DECIDERS\n",
           "-----------------\n",
           "These allow your strategies to have branching behaviour for a given condition.\n",

           "All deciders have to be given three algorithms to act upon and may take in extra input to function.\n",
           "Note that we mention algorithms, not actors, as it possible to nest deciders in one another as desired.\n",
           "The deciders that we have provided are listed below:\n",

           "GRADIENT:\ngradient peak-algorithm valley-algorithm neither-algorithm \n",
           "This checks to see if we have reached a peak or a valley and decide accordingly. We call the 'neither' ",
           "if we haven't reached a peak or a valley yet.\n",

           "RANDOM:\nrandom sell-chance buy-chance sell-algorithm buy-algorithm neither-algorithm\n",
           "sell-chance and buy-chance are float and when added together cannot exceed 1.0. However, if they are smaller ",
           "than 1.0, the neither-algorithm will be called for the remainder probability.\n",
           "A random number is generated, and if it falls within the 0 to sell-range it will call the sell-algorithm,\n",
           "and if the number falls within sell-range to (sell-range + buy-range) it calls the buy-algorithm.\n",
           "Finally if the number falls outside of the range given by the two chances, it will call the neither-algorithm\n",

           "HISTORIC:\nhistoric sell-algorithm buy-algorithm neither-algorithm\n",
           "Historic checks if the current market average is at a historical high or a historical low for the analysis session\n",
           "If it is a historical high, it calls the sell-algorithm and if it is a historical low it calls the buy-algorithm.\n",
           "If it is neither, it calls the neither-algorithm\n",

           "-----------------\n",
           "ACTORS\n",
           "-----------------\n",
           ]

parse :: MainInput -> Either String (Header, V.Vector OrderBookEntry) -> IO()
parse input fields = either (\x -> putStrLn "Failed to parse the provided file.\nPlease check for corruption.") (undefined) fields

{-
    This calls our functionParse function and is here just to abstract away from what functionParse needs and what it returns.
-}

createStrategy :: String -> (TraderState -> TraderState)
createStrategy s = snd $ functionParse (V.fromList $ words s) 0

{-
    This is the preliminary recursive function that we shall use for generating our strategies.
    Currently this can parse vectors correctly, but would be slow for very long strings (Due to no pattern matching).
    It'd be possible to improve this, but since this only gets called at the start (and never afterwards)
    I don't feel it's worth the effort to improve.
-}
functionParse :: V.Vector String -> Int -> (Int, (TraderState -> TraderState))
functionParse list loc
    | loc >= V.length list = error "Error in parsing function. Exceeded bounds."
    | loc < 0 = error "Error in parsing. Underflow."
    | otherwise = do
        if list V.! loc == "gradient" then do
            let peak = functionParse list $ loc+1
                valley = functionParse list (fst peak)
                neither = functionParse list (fst valley)
            (fst neither ,gradientSwitch (snd peak) (snd valley) (snd neither))
        else if list V.! loc == "nothing" then (loc+1, nothing)
        else if list V.! loc == "bestBuy" then (loc+1, nothing)
        else if list V.! loc == "bestSell" then (loc+1, nothing)
        else
            error $ "Invalid function name: "++(list V.! loc)

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
