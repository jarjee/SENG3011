--This file will hold the code required to do all the trade generation
module Trader (generateTrades, Strategy(..)) where

generateTrades = undefined

--Whatever you want, I've just put this in place so it can compile

data Strategy = TradeBadly Int | TradeLimit Int Int deriving (Show, Eq, Read)
