module Game.AbilityEffects where

import Grammar.Grammar
import Grammar.PrettyPrint
import Cards.Cards

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
    ability (CUnit _ row MoraleBoost _) = length (filter (cardInRow row) cards) - 1
    ability (c@(CUnit _ row Bond damage)) =
      damage * (length (filter (c ==) cards) - 1)
    ability (CUnit _ _ Horn _) = getTotalDamage (filter (cardInRow r) cards)
    ability (CUnit name row (Hero a) damage) = ability (CUnit name row a damage)
    ability (CLeader Siegemaster) = getTotalDamage (filter (cardInRow 3)
    ability _ = 0

cardInRow :: Row -> Card -> Bool
cardInRow r (CUnit _ row _ _) = r == row
cardInRow r _ = False

evalAbility :: Board -> Card -> Board
evalAbility b (CUnit _ _ ability _) = evalAbility' ability
  where evalAbility' Scorch = evalScorch b 1
evalAbility b (CLeader leader) = evalLeader leader
  where evalLeader SteelForged = evalScorch b 3
evalAbility b _ = b

evalScorch :: Board -> Row -> Board
evalScorch board r =
  if (getTotalDamage cardsInRow >= 10) then
    if (isAsTurn board) then
      board { b = discardMaxCard b r }
    else
      board { a = discardMaxCard a r }
  else
    board

discardMaxCard :: Player -> Row -> Player
discardMaxCard p r =
  p { cardsOnBoard = cards', usedCards = maxCard : (usedCards p) }
  where
    cards' = removeCard maxCard (cardsOnBoard p)
    maxCard = maxDamageCard (filter (cardInRow r) (cardsOnBoard p))

updateCurPlayer :: Board -> (Player -> Player) -> Board
updateCurPlayer board func =
  if (isAsTurn board) then
    (a board)
  else
    (b board)

removeCard :: Card -> [Card] -> [Card]
removeCard _ [] = []
removeCard card (c:cs) =
  if card == c then
    cs
  else
    c : (removeCard card cs)

maxDamageCard :: [Card] -> Card
maxDamageCard (card:rem) = maxDamage rem card
  where
    maxDamage [] max = max
    maxDamage (c:cs) max =
      if (getDamage max < getDamage c) then
        maxDamage cs c
      else
        maxDamage cs max

{-

data Leader =
    SteelForged  -- Scorch Siege if enemies Siege strength is 10 or higher 
  | Siegemaster -- Horn on Siege row
  | NorthCommander -- Clear any Weather effects
  | KingTemeria -- Pick a Fog card from your deck and play it immediately
  | Relentless -- Draw card from opp discard pile
  | WhiteFlame -- Cancel opp Leader ability
  | EmperorNilfgaard -- Look at 3 random cards of opp hand
  | ImperialMajesty -- Pick a rain card from deck and play immediately
  | Canceled -- If ability has been canceled (by WhiteFlame)
  deriving (Show, Eq)

data Ability =
  | Scorch -- if opp cc units row >= 10, destroy strongest unit in row
  | Spy -- puts card in opp board, draw 2 cards
  | Hero Ability -- immune to abilities/special effects, has another ability 
  | Medic -- play unit from used pile
  | Agile -- Can be played in range combat or close combat
  | Muster Name -- play all cards with same name from hand + deck right away
  | Decoy -- take card on board back into hand, replace it with the decoy
  | Horn Row -- choose a row, double strength of all cards in that row
  deriving (Show, Eq)

data Country =
    Nothern -- Draw extra card from deck after you win a round
  | Nilfgaard -- Win the game if it is a draw
  deriving (Show, Eq)

-}