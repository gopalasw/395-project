module Game.Turn where

import Data.List
import Text.Read

import Grammar.Grammar
import Grammar.Board
import Grammar.PrettyPrint
import Game.Basics
import Game.AbilityEffects
import Game.Round
import Cards.Cards

turnLoop :: IO Board -> IO Board
turnLoop board = do
  b <- board
  b <- playTurn $ pure b
  b <- pure $ evaluateTurn b
  putStrLn $ prettyPrintBoard b
  if not (roundOver b) then turnLoop (pure b) else return b

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
  where
    getCardHelper :: [Card] -> IO Card
    getCardHelper cs = do
      index <- getPlayIndex cs
      case getCard index cs of
        Just card -> return card
        Nothing   -> putStrLn "Invalid Input " >> getCardHelper cs

updatePlayedCard :: Player -> Card -> Player
updatePlayedCard p card =
  p { cardsOnBoard = card : (cardsOnBoard p),
      cardsInHand  = delete card (cardsInHand p)}

getPlayIndex :: [Card] -> IO Int
getPlayIndex = getIndex "Which card do you want to play?"

getSwapIndex :: [Card] -> IO Int
getSwapIndex = getIndex "Which card do you want to swap?"

getIndex :: String -> [Card] -> IO Int
getIndex s cs = do
  putStrLn $ show $ zip [1 .. (length cs)] (map (getName) cs)
  putStrLn s
  res <- getLineInt
  return res
  where
    getLineInt :: IO Int
    getLineInt = do
      line <- getLine
      case readMaybe line of
        Just x  -> return (x-1) -- List is index from 1 to X
        Nothing -> putStrLn "Invalid input" >> getLineInt

getName :: Card -> String
getName (CWeather n _)  = n
getName (CUnit n _ _ _) = n
getName (CLeader l)     = show l
getName CPass           = "Pass"
