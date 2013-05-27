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
import Evaluator

data MainInput = MainInput { inputCash :: Double,
                             algorithm :: (TraderState -> TraderState)}

main = do
    args <- getArgs
    if (length args == 3) then process args else helpMsg

process :: [String] -> IO()
process arg = do
    if "help" `elem` arg then helpMsg else do
        file <- readF $ head arg
        parse (MainInput (read $ arg !! 1) (createStrategy $ arg !! 2)) file

parse :: MainInput -> Either String (Header, V.Vector OrderBookEntry) -> IO()
parse input fields = either (\x -> putStrLn "Failed to parse the provided file.\nPlease check for corruption.") (\x -> putStrLn "BRAD FILE IS HERE.") fields

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

helpMsg :: IO()
helpMsg = do
    progName <- getProgName
    putStrLn $ concat ["Welcome to Team 6's High Frequency Trading simulator!\n",
           "Here's a run down of how to use this piece of software.\n",
           "Please execute the program on the commandline with all the information that is required.\n",

           progName, " name-of-file amount-of-money trading-algorithm\n",
           "Where name-of-file is the file that is going to be parsed,",
           " amount-of-money is the amount of money that you want to give the trader initially.\n",

           "\nThe trading-algorithm is slightly more complex as we let you build your custom strategies from \n",
           "building blocks that we provide. The algorithms are broken into 'Deciders' and 'Actors'.\n",
           "\n-----------------\n",
           "DECIDERS\n",
           "-----------------\n",
           "These allow your strategies to have branching behaviour for a given condition.\n",

           "All deciders have to be given three algorithms to act upon and may take in extra input to function.\n",
           "Note that we mention algorithms, not actors, as it possible to nest deciders in one another as desired.\n",
           "The deciders that we have provided are listed below:\n",

           "\nGRADIENT:\nFormat: gradient peak-algorithm valley-algorithm neither-algorithm \n",
           "This checks to see if we have reached a peak or a valley and decide accordingly. We call the 'neither' ",
           "if we haven't reached a peak or a valley yet.\n",

           "\nRANDOM:\nFormat: random sell-chance buy-chance sell-algorithm buy-algorithm neither-algorithm\n",
           "sell-chance and buy-chance are decimal numbers and when added together cannot exceed 1.0. \nHowever, if they are smaller ",
           "than 1.0, the neither-algorithm will be called for the remainder probability.\n",
           "A random number is generated from between 0.0 and 1.0, and if it falls within the 0 <-> sell-range it will call the sell-algorithm,\n",
           "and if the number falls within sell-range <-> (sell-range + buy-range) it calls the buy-algorithm.\n",
           "Finally if the number falls outside of the range given by the two chances, (sell-range + buy-range) <-> 1.0",
           " it will call the neither-algorithm\n",

           "\nHISTORIC:\nFormat: historic sell-algorithm buy-algorithm neither-algorithm\n",
           "Historic checks if the current market average is at a historical high or a historical low for the analysis session\n",
           "If it is a historical high, it calls the sell-algorithm and if it is a historical low it calls the buy-algorithm.\n",
           "If it is neither, it calls the neither-algorithm\n",

           "\n-----------------\n",
           "ACTORS\n",
           "-----------------\n",
           "These perform the purchasing and selling on the behalf of your deciders. These should be the 'endpoints' of your\n",
           "chosen strategies. They are intended to be what actually dictates the actions performed by the strategy.",
           "\n\nBESTBUY:\nFormat: bestBuy\nThis actor will generate a signal for the best purchase option available when called\n",
           "by choosing the ask with the lowest price. If there are no available entries or money is insufficient,\n",
           "it does not buy anything.",

           "\n\nBESTSELL:\nFormat: bestSell\nThis actor will generate a signal to sell the lowest value, held share to the highest\n",
           "priced bid. If no shares are held, it does not sell anything.",

           "\n\nNOTHING:\nFormat: nothing\nThis actor does not perform any action. For use when you do not necessarily want the trader\n",
           "to perform an action at a given point.",

           "\n-----------------\n",
           "USING DECIDERS AND ACTORS IN THE ALGORITHM FIELD\n",
           "-----------------\n",
           "Now that you know the components of a strategy, here are some examples of sample strategies:\n",
           
           "\nGRADIENT PURCHASING\n",
           "trading-algorithm = \"gradient bestSell bestBuy nothing\"\n",
           
           "\nRANDOM PURCHASING ON PEAK\n",
           "trading-algorithm = \"gradient random 0.3 0.7 bestBuy nothing nothing bestSell nothing\"\n",
           "alternate version = \"gradient (random 0.3 0.7 bestBuy nothing nothing) bestSell nothing\"\n",

           "\nRANDOMLY USE GRADIENT OR HISTORIC DECIDERS\n",
           "trading-algorithm = \"random 0.5 0.2 gradient bestSell bestBuy nothing historic bestSell bestBuy nothing nothing\"\n",
           "alternate version = \"random 0.5 0.2 (gradient bestSell bestBuy nothing) (historic bestSell bestBuy nothing) nothing\"\n",
           "\nThe algorithm field will ignore brackets, but it is good practice to use them to increase legibility.\n",
           "An example of using our program is thus:\n\n",

           progName, " input.csv 10000 \"gradient bestSell bestBuy nothing\"" 
           ]
