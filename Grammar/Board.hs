module Grammar.Board where

import Grammar.Grammar

import System.Random

-- The data structure to store the state of a game board
data Board = Board { a :: Player,
                     b :: Player,
                     weather :: Weather,
                     roundScore :: (Int, Int),
                     isATurn :: Bool,
                     randomSeed :: StdGen }
                   deriving (Show)

-- A convenience datatype for grabbing player A or B
data ABPlayer = A | B

--A triple of bools corresponding to whether there is weather
--affecting the three rows on the board
type Weather = (Bool, Bool, Bool)

getPlayer :: Board -> ABPlayer -> Player
getPlayer (Board p1 _  _ _ _ _) A = p1
getPlayer (Board _  p2 _ _ _ _) B = p2

-- Sets the corresponding row affected by this type of weather to True (unless
-- the weather is cleared, in which case all rows are set to False)
updateWeather :: Board -> Card -> Board
updateWeather board (c@(CWeather _ r)) = board { weather = updateRow (weather board) }
  where
    updateRow (x, y, z) =
      case r of
        0         -> (False, False, False)
        1         -> (True, y, z)
        2         -> (x, True, z)
        3         -> (x, y, True)
        otherwise -> (x, y, z)
updateWeather board _ = board
