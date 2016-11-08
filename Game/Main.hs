module Game.Main where

import Control.Applicative
import System.Random
import Data.Time.Clock

main = do
  t <- getCurrentTime
  putStrLn $ show $ fst $ next $ mkStdGen $ floor $ utctDayTime t

