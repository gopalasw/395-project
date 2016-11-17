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
  board <- pure $ brd t
  board <- roundSeq $ roundSeq $ pure board
  if (gameOver board)
  then
    putStrLn $ gameEnd board
    else do -- If the game isn't over, play the last round.
    board <- roundSeq $ pure board
    putStrLn $ gameEnd board
  putStrLn $ prettyPrintBoard board
  where
    seed :: UTCTime -> StdGen
    seed t = mkStdGen $ floor $ utctDayTime t
    brd t = initBoard (seed t) (Northern, Northern) ((CLeader Relentless), (CLeader Canceled))


gameEnd :: Board -> String
gameEnd board =
  case evalGame board of
    (True,  False) -> "Game Over, the winner is player 1.\n"
    (False, True)  -> "Game Over, the winner is player 2.\n"
    (False, False) -> "Game Over, no one wins in this game.\n"
    (True,  True)  -> error "Game error: two winners.\n"


gameOver :: Board -> Bool
gameOver (Board p1 p2 _ _ _ _) =
  (sameNGamesPlayed && (wonByP1 == 2 || wonByP2 == 2)) || --Either player won two rounds
  (sameNGamesPlayed && (gamesPlayedP1 == 2 || gamesPlayedP1 == 3))
  && (wonByP1 == 0 && wonByP2 == 0) || --Both players have had at least 2 draws
  (sameNGamesPlayed && (gamesPlayedP1 == 3) && (wonByP1 == 1) && (wonByP2 == 1))
    -- Both players won one round each, and then had a draw
    where
      wonByP1 = foldl (+) 0 (lives p1)
      wonByP2 = foldl (+) 0 (lives p2)
      gamesPlayedP1 = length (lives p1)
      gamesPlayedP2 = length (lives p2)
      sameNGamesPlayed = gamesPlayedP1 == gamesPlayedP2


evalGame :: Board -> (Bool, Bool)
evalGame (Board p1 p2 _ _ _ _)
  | wonByP1 > wonByP2  = (True, False)
  | wonByP2 > wonByP1  = (False, True)
  | wonByP2 == wonByP1 = (False, False)
  | otherwise          = (True, True)
  where
    wonByP1 = foldl (+) 0 (lives p1)
    wonByP2 = foldl (+) 0 (lives p2)
