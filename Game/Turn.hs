module Game.Turn where

import Control.Applicative
import Data.List
import System.Random
import Text.Read

import Grammar.Grammar
import Grammar.Board
import Grammar.PrettyPrint
import Game.Basics
import Game.AbilityEffects
import Cards.Cards
import Game.UserInput

turnLoop :: IO Board -> IO Board
turnLoop board = do
  b <- playTurn board
  b <- pure $ evaluateTurn b
  putStrLn $ "\n" ++  prettyPrintBoard b
  if not (roundOver b)
  then turnLoop (pure b)
  else putStrLn "\n----------Round over----------\n\n" >> return b


roundOver :: Board -> Bool
roundOver (Board p1 p2 _ _ _ _) =
  if (noCardsPlayed p1) || (noCardsPlayed p2) then False else (lastIsPass p1) && (lastIsPass p2)


otherPlayerPassed :: Board -> Bool
otherPlayerPassed (Board p1 p2 _ _ pTurn _) = if (noCardsPlayed otherP) then False else lastIsPass otherP
  where otherP = if pTurn then p2 else p1


noCardsPlayed :: Player -> Bool
noCardsPlayed p = [] == cardsOnBoard p


lastIsPass :: Player -> Bool
lastIsPass p = (head $ cardsOnBoard $ p) == CPass


evaluateTurn :: Board -> Board
evaluateTurn currentB@(Board p1 p2 w _ pTurn _) =
  cardsAbilityDamage $ currentB {
    roundScore = (totalDamage p1, totalDamage p2),
    isATurn    = if otherPlayerPassed currentB then pTurn else not pTurn
  }
  where
    totalDamage p = getTotalDamage (cardsOnBoard p) w


swapWhoseTurn :: Board -> Board
swapWhoseTurn currentB@(Board _ _ _ _ pTurn _) =
  currentB {
    isATurn = not pTurn
  }


updateRandomSeed :: Board -> Board
updateRandomSeed b = b { randomSeed = g }
  where
    seed   = randomSeed b
    (i, g) = next seed


playTurn :: IO Board -> IO Board
playTurn board = do
  board' <- board
  card <- if isAI board'
          then getCardAI (curHand board') (randomSeed board')
          else getCardHelper (curHand board') getPlayIndex
  board' <- pure $ updateRandomSeed board'
  evalAbility (updateCurPlayer (updateWeather board' card)
                               (updatePlayedCard card)) card
  where
    getCurHand board' =
      if isATurn board' then
        (cardsInHand $ a board')
      else
        (cardsInHand $ b board')
    curLeader board' =
      if isATurn board' then
        leaderHelper (leader $ a board')
      else leaderHelper (leader $ b board')
    leaderHelper (leaderCard, b) =
      if b then [] else [leaderCard]
    isAI board' = if isATurn board' then (isComp $ a board') else isComp $ b board'
    curHand board' = (getCurHand board') ++ (curLeader board') ++ [CPass]


getCardAI :: [Card] -> StdGen-> IO Card -- TODO: add a heuristic
getCardAI d g = do
  pure $ head s
  where
    (idx, _) = randomR (0, length d - 1) g
    (f, s)   = splitAt idx d


updatePlayedCard :: Card -> Player -> Player
updatePlayedCard CPass p =
  p { cardsOnBoard = CPass : (cardsOnBoard p),
      cardsInHand  = cardsInHand p }
updatePlayedCard (c@(CWeather _ _)) p =
  p { cardsInHand  = delete c (cardsInHand p) }
updatePlayedCard (CLeader _) p = p
updatePlayedCard card p =
  p { cardsOnBoard = card : (cardsOnBoard p),
      cardsInHand  = delete card (cardsInHand p)}
