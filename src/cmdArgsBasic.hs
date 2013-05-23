{-# LANGUAGE DeriveDataTypeable #-}
module SengBasic where
import System.Console.CmdArgs

data Algorithm  = Gradient {buyAlg :: Algorithm, sellAlg :: Algorithm, neither :: String}
		| Random {buyChance :: Double, sellChance :: Double, buyAlg :: Algorithm, sellAlg :: Algorithm, neither :: String}
		| End {algo :: String}
		  deriving (Show, Data, Typeable)

instance Default Algorithm => Algorithm

{-
data Gradient = Gradient {buyAlg :: , sellAlg :: , neither :: }
		deriving (Show, Data, Typeable)

data Random = Random {buyChance :: Double, sellChance :: Double, buyAlg :: , sellAlg :: , neither :: }
-}

-- def is the default for that type
-- help is for the information shown when you go program --help
-- typ is for the type of input

gradient = Gradient
		{buyAlg = def &= typ "ALGORITHM" &= help "The algorithm for buying"
		,sellAlg = def &= typ "ALGORITHM" &= help "The algorithm for selling"
		,neither = def &= typ "NOTHING" &= help "The option for \"Do nothing\""}

random = Random
		{buyChance = def &= typ "DOUBLE" &= help "The chance of buying"
		,sellChance = def &= typ "DOUBLE" &= help "The chance of selling"
		,buyAlg = def &= typ "ALGORITHM" &= help "The algorithm for buying"
		,sellAlg = def &= typ "ALGORITHM" &= help "The algorithm for selling"
		,neither = def &= typ "NOTHING" &= help "The option for \"Do nothing\""}

end = End {algo = def}

main = print =<< cmdArgs (modes [gradient,random])
