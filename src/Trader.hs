module Trader (
    traderEntry, TraderState(money,his,sha,avg),
    defaultTraderState, Share(shAmt,shaPri), gradientSwitch, nothing
    )
  where

import Types
import Data.Maybe
import Debug.Trace
import Data.List as L

{- |Takes in raw list of OrderBookEntry, filters so we only have ENTER entries.
    This is strictly a hack for getting the Trader submission in a useable state.
    Don't think we need to check for only unique entries (this is n^2, so I'd rather not)
-}
traderEntry :: [OrderBookEntry] -> [OrderBookEntry]
traderEntry list = filter ((== enter) . recordType) list

-- |This is the constant used to deal with floating point errors when calculating the second derative.
epsilon = 0.001

data TraderState = 
     TraderState { kn :: [OrderBookEntry],
                   knAsks :: [OrderBookEntry],
                   knLength :: Integer,
                   momtm :: Float,
                   money :: Float,
                   his :: [Share],
                   sha :: [Share],
                   avg :: [Double]} 
                   deriving (Show, Eq)

-- |Initialises a default traderState object so you don't have to.
defaultTraderState = TraderState [] [] 0 0.0 0.0 [] [] []

-- Handles the gradient logic for the traderBrain
gradientSwitch :: (TraderState -> f) -> (TraderState -> f) ->(TraderState -> f) -> TraderState -> f
gradientSwitch peak valley neither state = do
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

nothing :: TraderState -> TraderState
nothing state = state

randomSwitch :: (TraderState -> f) -> (TraderState -> f) -> (TraderState -> f) -> Float -> Float -> TraderState -> f
randomSwitch buy sell neither buyProb sellProb state = neither state

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

-- | This function allows us to compare prices of records with one another for sorting
priceComp fst snd
     | (maybe (99999) (id) (price fst))  < (maybe (99999) (id) (price snd)) = LT
     | (maybe (99999) (id) (price fst)) == (maybe (99999) (id) (price snd)) = EQ
     | (maybe (99999) (id) (price fst))  > (maybe (99999) (id) (price snd)) = GT
