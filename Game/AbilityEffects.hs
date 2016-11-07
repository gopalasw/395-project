module Game.AbilityEffects where

import Grammar.Grammar
import Grammar.PrettyPrint
import Game.Cards

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
    ability (CUnit _ _ (Horn r) _) = getTotalDamage (filter (cardInRow r) cards)
    ability (CUnit name row (Hero a) damage) = ability (CUnit name row a damage)
    ability (CLeader Siegemaster) = getTotalDamage (filter (cardInRow 3)
    ability _ = 0

cardInRow :: Row -> Card -> Bool
cardInRow r (CUnit _ row _ _) = r == row
cardInRow r _ = False

getTotalDamage :: [Card] -> Int 
getTotalDamage ls = 
   foldl (+) 0 (map getDamage ls)

getDamage :: Card -> Int
getDamage (CUnit _ _ _ d) = d 
getDamage _               = 0
