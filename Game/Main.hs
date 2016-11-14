module Game.Main where

import Cards.Cards
import Game.Basics
import Game.Init
import Game.Round
import Grammar.Grammar
import Grammar.PrettyPrint

import Control.Applicative
import System.Random
import Data.Time.Clock

main = do
  t <- getCurrentTime
  board <- pure $ board t
  board <- roundSeq $ roundSeq $ pure board
  -- Is the game over? If so, then another round.
  -- board <- roundSeq $ pure board
  putStrLn $ prettyPrintBoard board
  where
    seed :: UTCTime -> StdGen
    seed t = mkStdGen $ floor $ utctDayTime t
    board t = initBoard (seed t) (Northern, Northern) ((CLeader Relentless), (CLeader Canceled))
