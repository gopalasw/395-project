module Game.Main where

import Game.Basics
import Cards.Cards
import Game.Init
import Grammar.Grammar
import Grammar.PrettyPrint

import Control.Applicative
import System.Random
import Data.Time.Clock

main = do
  t <- getCurrentTime
  putStrLn $ prettyPrintBoard $
    initBoard (seed t) (Northern, Northern) ((CLeader Relentless), (CLeader Canceled))

  where
    seed t = snd $ next $ mkStdGen $ floor $ utctDayTime t
