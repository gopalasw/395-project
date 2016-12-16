module Main where

import Cards.Cards
import Game.Basics
import Game.Init
import Game.Round
import Game.UserInput
import Grammar.Grammar
import Grammar.PrettyPrint
import Grammar.Board
import Control.Applicative
import System.Random
import Data.Time.Clock

main = do
  t <- getCurrentTime
  -- board <- swapTwoCards (pure $ brd t)
  board <- pure $ brd t
  toss <- randomRIO (1,2) :: IO Int
  if (toss == 1) then do
    board <- pure $ board { isATurn = True }
    putStrLn "Player A will go first."
  else do
    board <- pure board { isATurn = False}
    putStrLn "Player B will go first."
  putStrLn "------------- Game Start -------------\n"
  putStrLn $ prettyPrintBoard board
  board <- roundSeq $ roundSeq $ pure board
  if (gameOver board)
  then
    putStrLn $ gameEnd board
  else do -- If the game isn't over, play the last round.
    board <- roundSeq $ pure board
    putStrLn $ gameEnd board
  where
    seed :: UTCTime -> StdGen
    seed t = mkStdGen $ floor $ utctDayTime t
    brd t = initVersusAIBoard (seed t) (Northern, Northern) ((CLeader Relentless), (CLeader NorthCommander))


swapTwoCards :: IO Board -> IO Board
swapTwoCards board = do
  board' <- board

  putStrLn "Both player can swap up to 2 cards\n"
  putStrLn "Player A swapping the first card\n"
  putStrLn $ prettyPrintCards (cardsInHand (a board'))
  card <- getCardHelper (CPass : (cardsInHand (a board'))) getSwapIndex
  board'' <- pure $ swapTwoCardsHelper board' 0 card

  putStrLn "Player A swappign the second card\n"
  putStrLn $ prettyPrintCards (cardsInHand (a board''))
  card' <- getCardHelper (CPass : (cardsInHand (a board''))) getSwapIndex
  board''' <- pure $ swapTwoCardsHelper board'' 0 card'

  putStrLn "Player B swapping the first card\n"
  putStrLn $ prettyPrintCards (cardsInHand (b board'''))
  card'' <- getCardHelper (CPass : (cardsInHand (b board'''))) getSwapIndex
  board'''' <- pure $ swapTwoCardsHelper board''' 1 card''

  putStrLn "Player B swapping the second card\n"
  putStrLn $ prettyPrintCards (cardsInHand (b board''''))
  card''' <- getCardHelper (CPass : (cardsInHand (b board''''))) getSwapIndex
  return $ swapTwoCardsHelper board'''' 1 card'''

swapTwoCardsHelper :: Board -> Int -> Card -> Board
swapTwoCardsHelper board i CPass = board
swapTwoCardsHelper board i card
  | i == 0 = board{a = swapOneCard (a board) (randomSeed board) card}
  | i == 1 = board{b = swapOneCard (b board) (randomSeed board) card}


gameEnd :: Board -> String
gameEnd board =
  case evalGame board of
    (True,  False) -> "Game Over, the winner is player A.\n"
    (False, True)  -> "Game Over, the winner is player B.\n"
    (False, False) -> "Game Over, no one wins in this game.\n"
    (True,  True)  -> error "Game error: two winners.\n"


gameOver :: Board -> Bool
gameOver (Board p1 p2 _ _ _ _) =
  (sameNGamesPlayed && (lostByP1 == 2 || lostByP2 == 2)) || --Either player lost two rounds
  (sameNGamesPlayed && (gamesPlayedP1 == 2 || gamesPlayedP1 == 3))
  && (wonByP1 == 0 && wonByP2 == 0) || --Both players have had 2 draws
  (sameNGamesPlayed && (gamesPlayedP1 == 3) && (wonByP1 == 1) && (wonByP2 == 1))
    -- Both players won one round each, and then had a draw
    where
      wonByP1          = foldl (+) 0 (lives p1)
      lostByP1         = foldl (\acc x -> if x  == 0 then (acc+1) else acc) 0 (lives p1)
      wonByP2          = foldl (+) 0 (lives p2)
      lostByP2         = foldl (\acc x -> if x  == 0 then (acc+1) else acc) 0 (lives p2)
      gamesPlayedP1    = length (lives p1)
      gamesPlayedP2    = length (lives p2)
      sameNGamesPlayed = gamesPlayedP1          == gamesPlayedP2


evalGame :: Board -> (Bool, Bool)
evalGame (Board p1 p2 _ _ _ _)
  | wonByP1 > wonByP2  = (True, False)
  | wonByP2 > wonByP1  = (False, True)
  | wonByP2 == wonByP1 = (False, False)
  | otherwise          = (True, True)
  where
    wonByP1 = foldl (+) 0 (lives p1)
    wonByP2 = foldl (+) 0 (lives p2)
