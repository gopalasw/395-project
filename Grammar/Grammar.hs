module Grammar.Grammar where

data Board = Board { a :: Player,
                     b :: Player,
                     weather :: [Card],
                     roundScore :: (Int, Int),
                     isATurn :: Bool }
                   deriving (Show)

data Player = Player { cardsInHand :: [Card],
                       cardsLeft :: [Card], 
                       cardsOnBoard :: [Card],
                       usedCards :: [Card],
                       score :: [Int],
                       leader :: Card,
                       country :: Country }
                     deriving (Show)

data Card =
    CWeather Name [Row]
  | CUnit Name Row Unit
  | CLeader Leader
  | CPass
  deriving (Show, Eq)

data Unit = Unit { ability :: Ability,
                   damage :: Int }
                 deriving (Show, Eq)

data Leader =
    SteelForged  -- Scorch Siege if enemies Siege strength is 10 or higher 
  | Siegemaster -- Horn on Siege row
  | NorthCommander -- Clear any Weather effects
  | KingTemeria -- Pick a Fog card from your deck and play it immediately
  | Relentless -- Draw card from opp discard pile
  | WhiteFlame -- Cancel opp Leader ability
  | EmperorNilfgaard -- Look at 3 random cards of opp hand
  | ImperialMajesty -- Pick a rain card from deck and play immediately
  deriving (Show, Eq)

data Country =
    Nothern -- Draw extra card from deck after you win a round
  | Nilfgaard -- Win the game if it is a draw
  deriving (Show, Eq)

data Ability =
    AMoraleBoost -- adds +1 to units in a single row, except for this card.
  | AScorch Row -- if opp cc units row >= 10, destroy strongest unit in row
  | ASpy -- puts card in opp board, draw 2 cards
  | AHero -- immune to abilities/special effects
  | ATightBond Name -- if beside same name card, strength of same name cards x2
  | AMedic -- play unit from used pile
  | AAgile -- Can be played in range combat or close combat
  | AMuster Name -- play all cards with same name from hand + deck right away
  | ADecoy -- take card on board back into hand, replace it with the decoy
  | AHorn -- choose a row, double strength of all cards in that row
  deriving (Show, Eq)


type Name = String -- Name of cards/countries/leaders
type Row = Int -- Rows that the card can be played on/affects
type Deck = (Country, [Card]) -- (Country name, relevant cards)
