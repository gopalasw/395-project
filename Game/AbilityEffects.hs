module Game.AbilityEffects where

import Grammar.Grammar
import Grammar.PrettyPrint
import Game.Cards

cardsDamage :: Board -> Board
cardsDamage board = board { roundScore = ((fst $ roundScore board) + aDamage, (snd $ roundScore board) + bDamage) }
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
      damage * ((sameName c cards) - 1) -- dont think this calculation is right..
    ability (CUnit _ _ Horn r _) = filter (cardInRow r) cards
    ability _ = 0

sameName :: Card -> [Card] -> Int
sameName _ [] = 0
sameName card (c:cs) = count + (sameName card cs)
  where count = if (c == card) then 1 else 0

cardInRow :: Row -> Card -> Bool
cardInRow r (CUnit _ row _ _) = r == row
cardInRow r _ = False
