module Game.AbilityEffects where

import Grammar.Grammar
  | CUnit Name Row Ability Int

abilityDamage :: Board -> Card -> Int
abilityDamage board card = ability card
  where
    cards = if (isATurn board) then (cardsOnBoard (a board)) else (cardsOnBoard (b board))
    ability (CUnit _ row MoraleBoost _) = length cards
    ability (CUnit _ row (Bond name) damage) =
      2 * damage * (sameName name cards) -- dont think this calculation is right..
    -- ability (CUnit _ _ Horn _) = 

sameName :: Name -> [Card] -> Int
sameName _ [] = 0
sameName name ((CUnit n _ _ _):cs) = count + (sameName name cs)
  where count = if (name == n) then 1 else 0
sameName _ _ = 0


  | Hero -- immune to abilities/special effects
  | Bond Name -- if beside same name card, strength of same name cards x2
  | Medic -- play unit from used pile
  | Agile -- Can be played in range combat or close combat
  | Muster Name -- play all cards with same name from hand + deck right away
  | Decoy -- take card on board back into hand, replace it with the decoy
  | Horn -- choose a row, double strength of all cards in that row
  | None -- No ability
  

