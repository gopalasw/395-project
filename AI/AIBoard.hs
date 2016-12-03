module AI.AIBoard where

import Grammar.Board
import AI.AIPlayer

boardToIA :: Board -> [Int]
boardToIA board = aToIA ++ bToIA ++ wToIA ++ rToIA 
  where
    aToIA = thisPlayerToIA $ a board
    bToIA = otherPlayerToIA $ b board
    wToIA = weatherToIA $ weather board
    rToIA = roundScoreToIA $ roundScore board


roundScoreToIA :: (Int, Int) -> [Int]
roundScoreToIA (i1, i2) = [i1, i2]


weatherToIA :: Weather -> [Int]
weatherToIA (b1, b2, b3) = [bToI b1, bToI b2, bToI b3]
  where 
    bToI :: Bool -> Int
    bToI True  = 1
    bToI False = 0
