module Game.Turn where

import Grammar.Grammar
import Data.List
import Game.Basics
import Cards.Cards
import Text.Read
import Game.AbilityEffects

evaluateTurn :: Board -> Board
evaluateTurn currentB@(Board p1 p2 _ _ pTurn _) =
  currentB {
    roundScore = (totalDamage p1, totalDamage p2),
    isATurn    = not pTurn
  }
  where
    totalDamage p = getTotalDamage (cardsOnBoard p)
  
playTurn :: IO Board -> IO Board
playTurn board = do
  board' <- board
  if isATurn board'
  then do 
    card <- getCardHelper (cardsInHand $ a board')
    evalAbility (board' { a = updatePlayedCard (a board') card }) card 
  else do 
    card <- getCardHelper (cardsInHand $ b board') 
    evalAbility (board' { b = updatePlayedCard (b board') card }) card 
  -- Cannot be simiplied.
  where
    getCardHelper :: [Card] -> IO Card
    getCardHelper cs = do
      index <- getIndex cs
      case getCard index cs of
        Just card -> return card
        Nothing   -> putStrLn "Invalid Input " >> getCardHelper cs

updatePlayedCard :: Player -> Card -> Player
updatePlayedCard p card = 
  p { cardsOnBoard = card : (cardsOnBoard p),
      cardsInHand  = delete card (cardsInHand p)}


getIndex :: [Card] -> IO Int
getIndex cs = do
  putStrLn $ show $ zip [1 .. (length cs)] (map (getName) cs)
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

getName :: Card -> String
getName (CWeather n _)  = n
getName (CUnit n _ _ _) = n
getName (CLeader l)     = show l
getName CPass           = "Pass"


