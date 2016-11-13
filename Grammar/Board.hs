module Grammar.Board where

import Grammar.Grammar

import System.Random

data Board = Board { a :: Player,
                     b :: Player,
                     weather :: [Bool],
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
