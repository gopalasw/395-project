module Grammar.Grammar where

data Board = Board { a :: Player,
                     b :: Player,
                     weather :: [Card],
                     roundScore :: (Int, Int) }

data Player = Player { cardsInHand :: [Card],
                       cardsLeft :: [Card], 
                       cardsOnBoard :: [Card],
                       usedCards :: [Card],
                       score :: [Int],
                       leaderAbility :: Ability,
                       countryAbility :: Ability }


data Card = CWeather Name [Row] | CUnit Name Row Unit
data Unit = Unit { ability :: Ability,
                   damage :: Int }

data Ability =
    AMoraleBoost -- adds +1 to units in a single row, except for this card.
  | AScorch -- if opp cc units row >= 10, destroys strongest unit in that row
  | ASpy -- puts card in opp board, draw 2 cards
  | AHero -- immune to abilities/special effects
  | ATightBond Name -- if beside same name card, strength of same name cards x2
  | AMedic -- play unit from used pile
  | AAgile -- Can be played in range combat or close combat
  | AMuster Name -- play all cards with same name from hand + deck right away
  | ADecoy -- take card on board back into hand, replace it with the decoy
  | AHorn -- choose a row, double strength of all cards in that row

type Name = String -- Name of cards/countries/leaders
type Row = Int -- Rows that the card can be played on/affects
type Deck = (Name, [Card]) -- (Country name, relevant cards)
