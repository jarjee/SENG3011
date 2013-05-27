{-# LANGUAGE DoAndIfThenElse #-}
module Trader (
    TraderState(money,avg),TraderPromise(..),
    defaultTraderState, createStrategy, traderId
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
import Data.Heap as H

data TraderState = 
     TraderState { money :: Double,
                   avg :: [Double],
                   hisVal :: (Double, Double), --left is lowest, right is highest
                   sellHeap :: MaxPrioHeap Double OrderBookEntry,
                   buyHeap :: MinPrioHeap Double OrderBookEntry,
                   heldHeap :: MinPrioHeap Double OrderBookEntry, --Sell what we bought cheapest first
                   promises :: [TraderPromise],
                   tCurrTime :: String
                   } deriving (Show, Eq)

-- |Initialises a default traderState object so you don't have to.
defaultTraderState :: TraderState
defaultTraderState = TraderState 0.0 [] (infinity, negativeInfinity) H.empty H.empty H.empty [] ""

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
        if ((sellProb + buyProb) > 1) || (sellProb < 0) || (buyProb < 0) then neither state else do
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
{-
    We need a constant to identify the trader with.
    It's possible to come up with a better form of identification
    but since this is only a simple project it's 
    what we're going to use
-}

traderId = 42 -- So the Orderbook can identify this trade

data TraderPromise = BuyPromise { buyShare :: OrderBookEntry } | AskPromise {askShare :: OrderBookEntry } deriving (Show, Eq)

nothing :: TraderState -> TraderState
nothing state = state

-- Attempt to buy as much as the lowest priced stock as possible
-- Has checks to make sure we don't buy more than the stock available
bestBuy :: TraderState -> TraderState
bestBuy state = if H.isEmpty (buyHeap state) then state else
    maybe (state) (remainder . snd) bestPurchase
    where bestPurchase = viewHead (buyHeap state)
          remainder ent = state {promises = (buyPromise ent):(promises state), money = (money state)-(buyCost ent)}
          buyPromise h = BuyPromise $ makeEntry h (tCurrTime state) (buyPrice h) (buyAmount h) traderId 'B'
          canAfford h = truncate $ (money state) / (buyPrice h)
          buyAmount h = if (canAfford h) > (stockVolume h) then (stockVolume h) else canAfford h
          stockVolume h = (maybe (0) (id) $ volume h) 
          buyPrice h = maybe (0) (id) $ price h
          buyCost h = if (buyAmount h) > 0 then (fromIntegral $ buyAmount h) * (buyPrice h) else 0

-- Simply hawk off the cheapest stock we own at the highest price we know
bestSell :: TraderState -> TraderState
bestSell state = if H.isEmpty (heldHeap state) then state else
    maybe (state) (sellOrder) bestSell
    where bestSell = view (heldHeap state)
          bestBuyer = viewHead (sellHeap state)
          bestPrice ent = maybe (9999) (id) (price $ snd ent)
          sellOrder s = state {promises = (sellPromise (fst s) (maybe (fst s) (id) bestBuyer)):(promises state), heldHeap = snd s}
          sellPromise s b = AskPromise $ (snd s) {price = Just (bestPrice b)}

----------------------
-----  Strategy ------
----- Generation -----
----------------------

{-
    This calls our functionParse function and is here just to abstract away from what functionParse needs and what it returns.
    This also strips out all round brackets, to better match what our help flavour text says.
-}

createStrategy :: String -> (TraderState -> TraderState)
createStrategy s = snd $ functionParse (V.fromList $ words $ L.filter (\x-> x /= '(' && x /= ')' ) s) 0

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
        else if list V.! loc == "bestBuy" then (loc+1, bestBuy)
        else if list V.! loc == "bestSell" then (loc+1, bestSell)
        else
            error $ "Invalid function name: "++(list V.! loc)
