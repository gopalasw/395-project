module Game.Turn where

import Grammar.Grammar
import Data.List
import Game.Basics
import Cards.Cards
import Text.Read
evaluateTurn :: Board -> Board
evaluateTurn currentB@(Board p1 p2 _ _ pTurn) =
  currentB {
    roundScore = (totalDamage p1, totalDamage p2),
    isATurn    = not pTurn
  }
  where
    totalDamage p = getTotalDamage (cardsOnBoard p)
  
playTurn :: IO Board -> IO Board
playTurn = undefined{--do
  b' <- b
  

  case card  of
    CWeather name r -> (weather b') 
      
    CUnit name r ability damage
    CLeader leader
    CPass ->
  
  see which player is playing
  ask for user to choose card
  pattern match on the chosen card 
  return the updated board
--}
-- TODO
-- 1. Which palyer is playing
-- 2. Add the card to board (for abilities, b' <- addCardToBoard
--    then change fields in b' and return b'
{--
addCardToBoard :: Board -> Card -> IO Board
addCardToBoard b card@(CWeather name row) 
  | clear     = return $ b {(weather b) = [False, False, False]}
  | otherwise = changeElemebtByIndex (weather b) row True
addCardToBoard b (CUnit name row ability damage) = 
  abilityToBoard b ability 
addCardToBoard b (CLeader leader) =
addCardToBoard b (CPass) = 
  -- Check who is playing first
  (CPass) : (cardsOnBoard p) 
  Call cards.evalAbility function to change the board
--}

playCard :: Player -> Int -> Player
playCard p i = 
  p { cardsOnBoard = cardPlayed : (cardsOnBoard p),
      cardsInHand  = delete cardPlayed (cardsInHand p)}
  where
    cardPlayed :: Card
    cardPlayed = getPlayedCard (cardsInHand p) i

getIndex :: IO Int
getIndex = do
  --putStrLn $ show $ zip [1 .. (length cs)] (map (getName) cs)
  putStrLn "Which card do you want to play?"
  res <- getLineInt
  return res
  where 
    getLineInt :: IO Int
    getLineInt = do
      line <- getLine
      case readMaybe line of 
        Just x -> return x
        Nothing -> putStrLn "Invalid input" >> getLineInt


getPlayedCard :: [Card] -> Int -> Card
getPlayedCard cs i =  head (drop (i - 1) cs)

getName :: Card -> String
getName (CWeather n _)  = n
getName (CUnit n _ _ _) = n
getName (CLeader l)     = show l
getName CPass           = "Pass"


