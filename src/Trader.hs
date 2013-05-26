module Trader (
    TraderState(money,his,sha,avg),
    defaultTraderState, Share(shAmt,shaPri), createStrategy
    )
  where

import Types
import Data.Maybe
import Debug.Trace
import Data.List as L
import System.IO.Unsafe
import System.Random
import Data.Number.Transfinite
import qualified Data.Vector as V

data TraderState = 
     TraderState { kn :: [OrderBookEntry],
                   knAsks :: [OrderBookEntry],
                   knLength :: Integer,
                   momtm :: Float,
                   money :: Float,
                   his :: [Share],
                   sha :: [Share],
                   avg :: [Double],
                   hisVal :: (Double, Double)} --left is lowest, right is highest
                   deriving (Show, Eq)

-- |Initialises a default traderState object so you don't have to.
defaultTraderState = TraderState [] [] 0 0.0 0.0 [] [] [] (infinity, negativeInfinity)

-------------------------------
----------- DECIDERS ----------
-------------------------------
gradientSwitch :: (TraderState -> f) -> (TraderState -> f) ->(TraderState -> f) -> TraderState -> f
gradientSwitch peak valley neither state = if (length $ avg state) < 3 then neither state else do
    let entries = avg state
        first = entries !! 0
        second = entries !! 1
        third = entries !! 2
        gradient = (first - second)
        secGradient = (second - third)
    if (gradient/secGradient) <= 0 then do
        --We've reached a peak or a valley.
        if (secGradient > 0) then peak state else valley state
    else
        neither state

randomSwitch :: Float -> Float -> (TraderState -> f) -> (TraderState -> f) -> (TraderState -> f) -> TraderState -> f
randomSwitch sellProb buyProb sell buy neither state = do
    let x = unsafePerformIO $ randomRIO (0.0, 1.0)
    if x <= sellProb then sell state else if (x > sellProb) && (x < sellProb+buyProb) then buy state else neither state

historicSwitch :: (TraderState -> f) -> (TraderState -> f) -> (TraderState -> f) -> TraderState -> f
historicSwitch sell buy neither state = if (length $ avg state) < 1 then neither state else do
    let entries = avg state
        recorded = hisVal state
        current = entries !! 0 
    if current < fst recorded then buy state {hisVal = (current, snd recorded)} 
    else if current > snd recorded then sell state {hisVal = (fst recorded, current)}
    else neither state

--------------------------------
-------- ACTORS ----------------
--------------------------------

nothing :: TraderState -> TraderState
nothing state = state

data BoughtShares = BoughtShares { remShares :: [OrderBookEntry],
                remMoney :: Double,
                bouSha :: Share} deriving (Show, Eq)

data Share = Share { shAmt :: Integer,
        shaPri :: Double } deriving (Show, Read, Eq)

{-| This is a simple buying strategy that takes in a list of Asks and attempts buy as many items as possible.
    It assumes that it can buy as many records as it wants and in its current form does not generate trade signals.
    It also removes records once it finds a suitable match. The final version of this function will need to update
    the entries according to the volume bought.
-}
buyShares :: [OrderBookEntry] -> Double -> BoughtShares
buyShares [] money = BoughtShares [] money $ Share 0 0
buyShares list money = do
    if list /= [] then do
        let bestPrice = head list
            bestPriceCost = maybe (0) (id) (price bestPrice)
            canBuy = truncate $ money / bestPriceCost
        if canBuy > 0 then do
            let shareCost = (fromIntegral canBuy) * bestPriceCost
                remainingMoney = money - shareCost
                remainingList = L.delete bestPrice list 
            BoughtShares remainingList remainingMoney $ Share (toInteger canBuy) bestPriceCost
        else BoughtShares list money $ Share 0 0
    else BoughtShares list money $ Share 0 0 

----------------------
-----  Strategy ------
----- Generation -----
----------------------

{-
    This calls our functionParse function and is here just to abstract away from what functionParse needs and what it returns.
    This also strips out all round brackets, to better match what our help flavour text says.
-}

createStrategy :: String -> (TraderState -> TraderState)
createStrategy s = snd $ functionParse (V.fromList $ words $ filter (\x-> x /= '(' && x /= ')' ) s) 0

{-
    This is the preliminary recursive function that we shall use for generating our strategies.
    Currently this can parse vectors correctly, but would be slow for very long strings (Due to no pattern matching).
    It'd be possible to improve this, but since this only gets called at the start (and never afterwards)
    I don't feel it's worth the effort to improve.
-}
functionParse :: V.Vector String -> Int -> (Int, (TraderState -> TraderState))
functionParse list loc
    | loc >= V.length list = error "Algorithm given was not balanced. Please check if you're missing an input to a decider."
    | loc < 0 = error "Error in parsing. Underflow."
    | otherwise = do
        if list V.! loc == "gradient" then do
            let peak = functionParse list $ loc+1
                valley = functionParse list (fst peak)
                neither = functionParse list (fst valley)
            (fst neither ,gradientSwitch (snd peak) (snd valley) (snd neither))
        else if list V.! loc == "random" then do
            let sellChance = read $ list V.! (loc+1)
                buyChance = read $ list V.! (loc+2)
                sellFunc = functionParse list $ (loc+3)
                buyFunc = functionParse list (fst sellFunc)
                neither = functionParse list (fst buyFunc)
            (fst neither, randomSwitch sellChance buyChance (snd sellFunc) (snd buyFunc) (snd neither))
        else if list V.! loc == "historic" then do
            let sellFunc = functionParse list $ loc+1
                buyFunc = functionParse list (fst sellFunc)
                neither = functionParse list (fst buyFunc)
            (fst neither, historicSwitch (snd sellFunc) (snd buyFunc) (snd neither))
        else if list V.! loc == "nothing" then (loc+1, nothing)
        else if list V.! loc == "bestBuy" then (loc+1, nothing)
        else if list V.! loc == "bestSell" then (loc+1, nothing)
        else
            error $ "Invalid function name: "++(list V.! loc)
