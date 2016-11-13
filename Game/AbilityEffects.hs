module Game.AbilityEffects where

import Grammar.Grammar
import Grammar.PrettyPrint
import Cards.Cards
import Game.Basics
import Data.List
import System.Random
import Grammar.Board


-- updates board with additional damage caused by abilities
cardsAbilityDamage :: Board -> Board
cardsAbilityDamage board = board { roundScore = ((fst $ roundScore board) + aDamage, (snd $ roundScore board) + bDamage) }
  where
    aCards = (cardsOnBoard (a board))
    bCards = (cardsOnBoard (b board))
    aDamage = sum $ map (abilityDamage board aCards) aCards
    bDamage = sum $ map (abilityDamage board bCards) bCards


abilityDamage :: Board -> [Card] -> Card -> Int
abilityDamage board cards card = ability card
  where
    ability (CUnit _ row Morale _) = length (filter (cardInRow row) cards) - 1
    ability (c@(CUnit _ row Bond damage)) = damage * (length (filter (c ==) cards) - 1)
    ability (CUnit _ _ (Horn r) _) = getTotalDamage (filter (cardInRow r) cards)
    ability (CUnit name row (Hero a) damage) = ability (CUnit name row a damage)
    ability (CLeader Siegemaster) = getTotalDamage (filter (cardInRow 3) cards)
    ability _ = 0


cardInRow :: Row -> Card -> Bool
cardInRow r (CUnit _ row _ _) = r == row
cardInRow r (CWeather _ row) = r == row
cardInRow r _ = False


evalAbility :: Board -> Card -> IO Board
evalAbility board (c@(CUnit _ _ ability _)) = evalAbility' ability
  where
    evalAbility' Scorch = return $ evalScorch board 1
    evalAbility' Spy = return $ evalSpy board c
    evalAbility' (Hero ability) = evalAbility' ability
    evalAbility' Medic = undefined
    evalAbility' Agile = undefined
    evalAbility' Muster = return $ updateCurPlayer board (muster c)
    evalAbility' Decoy = undefined
    evalAbility' (Horn _) = return $ updateHorn board c row
    evalAbility' _ = return board
evalAbility board (CLeader leader) = evalLeader leader
  where
    evalLeader SteelForged = return $ evalScorch board 3
    evalLeader NorthCommander = undefined
    evalLeader KingTemeria = undefined
    evalLeader Relentless = undefined
    evalLeader WhiteFlame = undefined
    evalLeader EmperorNilfgaard = undefined
    evalLeader ImperialMajesty = undefined
    evalLeader _ = return board
evalAbility board _ = return board


evalScorch :: Board -> Row -> Board
evalScorch board r =
  if (getTotalDamage cardsInRow >= 10) then
    updateOppPlayer board (discardMaxCard r)
  else
    board
  where
    cardsInRow =
      if (isATurn board) then
        (cardsOnBoard (b board))
      else (cardsOnBoard (a board))


evalSpy :: Board -> Card -> Board
evalSpy board spy = updateCurPlayer (updateOppPlayer board (playSpy spy)) ((removeSpy spy) . (drawTwo (randomSeed board)))


playSpy :: Card -> Player -> Player
playSpy card p =
  p { cardsOnBoard = card : (cardsOnBoard p) }


removeSpy :: Card -> Player -> Player
removeSpy spy p =
  p { cardsOnBoard = delete spy (cardsOnBoard p) }


drawTwo :: StdGen -> Player -> Player
drawTwo seed p =
  p { cardsInHand = (fst cards) ++ (cardsInHand p),
      usedCards = snd cards }
  where
    cards =
      case drawCardsR seed 2 (usedCards p) of
      (Just drew, left) -> (drew, left)
      (Nothing,   left) -> ([],   left)


discardMaxCard :: Row -> Player -> Player
discardMaxCard r p =
  p { cardsOnBoard = cards', usedCards = maxCard : (usedCards p) }
  where
    cards' = delete maxCard (cardsOnBoard p)
    maxCard = maxDamageCard (filter (cardInRow r) (cardsOnBoard p))


maxDamageCard :: [Card] -> Card
maxDamageCard (card:rem) = maxDamage rem card
  where
    maxDamage [] max = max
    maxDamage (c:cs) max =
      if (getDamage max < getDamage c) then
        maxDamage cs c
      else
        maxDamage cs max


muster :: Card -> Player -> Player
muster c p = p { cardsOnBoard = (cardsOnBoard p) ++ musterCards,
                    cardsLeft    = filter (c /=) (cardsLeft p),
                    cardsInHand  = filter (c /=) (cardsInHand p) }
  where musterCards = filter (c ==) ((cardsLeft p) ++ (cardsInHand p))

{-

data Leader =
  | NorthCommander -- Clear any Weather effects
  | KingTemeria -- Pick a Fog card from your deck and play it immediately
  | Relentless -- Draw card from opp discard pile
  | WhiteFlame -- Cancel opp Leader ability
  | EmperorNilfgaard -- Look at 3 random cards of opp hand
  | ImperialMajesty -- Pick a rain card from deck and play immediately
  | Canceled -- If ability has been canceled (by WhiteFlame)
  deriving (Show, Eq)

data Ability =
  | Spy -- puts card in opp board, draw 2 cards
  | Hero Ability -- immune to abilities/special effects, has another ability 
  | Medic -- play unit from used pile
  | Agile -- Can be played in range combat or close combat
  | Decoy -- take card on board back into hand, replace it with the decoy
  | Horn Row -- choose a row, double strength of all cards in that row
  deriving (Show, Eq)

data Country =
    Nothern -- Draw extra card from deck after you win a round
  | Nilfgaard -- Win the game if it is a draw
  deriving (Show, Eq)

-}