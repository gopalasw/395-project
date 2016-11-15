module Game.Main where

import Cards.Cards
import Game.Basics
import Game.Init
import Game.Round
import Grammar.Grammar
import Grammar.PrettyPrint
import Grammar.Board
import Control.Applicative
import System.Random
import Data.Time.Clock

main = do
  t <- getCurrentTime
  board <- pure $ board t
  board <- roundSeq $ roundSeq $ pure board
  -- Is the game over? If so, then another round.
  -- board <- roundSeq $ pure board
  if (gameOver board) 
  then 
    putStrLn $ gameEnd board 
  else do
    board <- roundSeq $ pure board
    putStrLn $ gameEnd board 
  
  putStrLn $ prettyPrintBoard board
  
  where
    seed :: UTCTime -> StdGen
    seed t = mkStdGen $ floor $ utctDayTime t
    board t = initBoard (seed t) (Northern, Northern) ((CLeader Relentless), (CLeader Canceled))

gameEnd :: Board -> String 
gameEnd board =
  case evalGame board of
    (True,  False) -> "Game Over, the winner is player 1"
    (False, True)  -> "Game Over, the winner is player 2"
    (False, False) -> "Game Over, no one wins in this game"
    (True,  True)  -> error "Game error: two winners"


gameOver :: Board -> Bool
gameOver (Board p1 p2 _ _ _ _) =
  (length (lives p1)) == (length (lives p2)) &&
  (length (lives p1)) == 3

evalGame :: Board -> (Bool, Bool)
evalGame (Board p1 p2 _ _ _ _) = 
  (((foldl (+) 0 (lives p1)) == 2), 
   ((foldl (+) 0 (lives p2)) == 2))
