module Game.UserInput where

import Grammar.Grammar
import Data.List
import Text.Read
import Game.Basics

-- helper to get cards, given an index
getCardHelper :: [Card] -> ([Card] -> IO Int) -> IO Card
getCardHelper cs f = do
  index <- f cs
  case getCard index cs of
    Just card -> return card
    Nothing   -> putStrLn "Invalid Input " >> getCardHelper cs f

-- prompts for different situations
getPlayIndex :: [Card] -> IO Int
getPlayIndex = getIndex "\nWhich card do you want to play?"

getDrawIndex :: [Card] -> IO Int
getDrawIndex = getIndex "\nWhich card do you want to draw?"

getSwapIndex :: [Card] -> IO Int
getSwapIndex = 
  getIndex "\nWhich card do you want to swap? (Pass for not swapping) \n"  

getRow :: IO Int
getRow = do
  putStrLn "\nWhich row do you want to play it on?"
  (fmap (+1) getLineInt)

-- helper for displaying card and respective index
getIndex :: String -> [Card] -> IO Int
getIndex s cs = do
  putStrLn $ show $ zip [1 .. (length cs)] (map (getName) cs)
  putStrLn s
  res <- getLineInt
  return res

getLineInt :: IO Int
getLineInt = do
  line <- getLine
  case readMaybe line of
    Just x  -> return (x-1) -- List is index from 1 to X
    Nothing -> putStrLn "Invalid input" >> getLineInt

-- helper function to get name of a card
getName :: Card -> String
getName (CWeather n _)  = n
getName (CUnit n _ _ _) = n
getName (CSpecial n _ _)  = n
getName (CLeader l)     = show l
getName CPass           = "Pass"
