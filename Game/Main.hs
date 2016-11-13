module Game.Main where

import Control.Applicative
import System.Random
import Data.Time.Clock
import Game.Basics
import Cards.Cards
import Game.Init
import Grammar.Grammar

main = do
  t <- getCurrentTime
  putStrLn $ show $
    initBoard (seed t) (Northern, Northern) ((CLeader Relentless), (CLeader Canceled))

  where
    seed t = snd $ next $ mkStdGen $ floor $ utctDayTime t
