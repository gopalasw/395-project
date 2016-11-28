module Game.Turn where

import Data.List
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
  b <- board
  b <- playTurn $ pure b
  b <- pure $ evaluateTurn b
  putStrLn $ prettyPrintBoard b
  if not (roundOver b) then turnLoop (pure b) else putStrLn "\nRound over.\n\n" >> return b
  -- TODO Print who won the round.

roundOver :: Board -> Bool
roundOver (Board p1 p2 _ _ _ _) =
  if (noCardsPlayed p1) || (noCardsPlayed p2) then False else (isPass p1) && (isPass p2)
  where
    noCardsPlayed :: Player -> Bool
    noCardsPlayed p = [] == cardsOnBoard p
    isPass :: Player -> Bool
    isPass p = (head $ cardsOnBoard $ p) == CPass

evaluateTurn :: Board -> Board
evaluateTurn currentB@(Board p1 p2 w _ pTurn _) =
  currentB {
    roundScore = (totalDamage p1, totalDamage p2),
    isATurn    = not pTurn
  }
  where
    totalDamage p = getTotalDamage (cardsOnBoard p) w

playTurn :: IO Board -> IO Board
playTurn board = do
  board' <- board
  card <- getCardHelper ((getCurHand board') ++ [CPass]) getPlayIndex
  evalAbility (updateCurPlayer (updateWeather board' card)
                               (updatePlayedCard card)) card
  where
    getCurHand board' =
      if isATurn board' then
        (cardsInHand $ a board')
      else
        (cardsInHand $ b board')

updatePlayedCard :: Card -> Player -> Player
updatePlayedCard CPass p =
  p { cardsOnBoard = CPass : (cardsOnBoard p),
      cardsInHand  = cardsInHand p }
updatePlayedCard (c@(CWeather _ _)) p =
  p { cardsInHand  = delete c (cardsInHand p) }
updatePlayedCard card p =
  p { cardsOnBoard = card : (cardsOnBoard p),
      cardsInHand  = delete card (cardsInHand p)}