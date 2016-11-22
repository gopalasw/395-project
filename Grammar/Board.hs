module Grammar.Board where

import Grammar.Grammar

import System.Random

data Board = Board { a :: Player,
                     b :: Player,
                     weather :: (Bool, Bool, Bool),
                     roundScore :: (Int, Int),
                     isATurn :: Bool,
                     randomSeed :: StdGen }
                   deriving (Show)

data ABPlayer = A | B

getPlayer :: Board -> ABPlayer -> Player
getPlayer (Board p1 _  _ _ _ _) A = p1
getPlayer (Board _  p2 _ _ _ _) B = p2

getStdGen :: Board -> StdGen
getStdGen (Board _ _ _ _ _ s) = s

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