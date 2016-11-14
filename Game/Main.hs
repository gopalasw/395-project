module Game.Main where

import Game.Basics
import Game.Turn
import Cards.Cards
import Game.Init
import Grammar.Grammar
import Grammar.PrettyPrint

import Control.Applicative
import System.Random
import Data.Time.Clock

main = do
  t <- getCurrentTime
  board <- pure $ board t
  putStrLn $ prettyPrintBoard board
  board <- turnLoop $ pure $ board
  putStrLn $ prettyPrintBoard board
  where
    seed :: UTCTime -> StdGen
    seed t = mkStdGen $ floor $ utctDayTime t
    board t = initBoard (seed t) (Northern, Northern) ((CLeader Relentless), (CLeader Canceled))

