module Grammar.Grammar where
import System.Random

data Player = Player { cardsInHand :: [Card],
                       cardsLeft :: [Card],
                       cardsOnBoard :: [Card],
                       usedCards :: [Card],
                       lives :: [Int], -- list of results for each round 
                                       -- (1 - win, 0 - loss)
                       leader :: (Card, Bool), -- a leader, and if it has been 
                                               -- used this game
                       country :: Country,
                       isComp :: Bool } -- human player or computer player
                     deriving (Show)

data Card =
    CWeather Name Row
  | CSpecial Name Row Ability
  | CUnit Name Row Ability Damage
  | CLeader Leader
  | CPass
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
  | Canceled -- If ability has been canceled (by WhiteFlame)
  deriving (Show, Eq)

-- determines which deck player draws from
data Country =
    Northern
  | Nilfgaard 
  deriving (Show, Eq)

data Ability =
    Morale -- adds +1 to units in a single row, except for this card.
  | Scorch -- if opp cc units row >= 10, destroy strongest unit in row
  | Spy -- puts card in opp board, randomly draw 2 cards
  | Hero Ability -- immune to abilities/special effects, has another ability
  | Bond -- if beside same name card, strength of same name cards x2
  | Medic -- play unit from used pile
  | Agile -- Can be played in range combat or close combat
  | Muster -- play all cards with same name from hand + deck right away
  | Decoy -- take card on board back into hand, replace it with the decoy
  | Horn -- choose a row, double strength of all cards in that row
  | None -- No ability
  deriving (Show, Eq)

-- making types for readability
type Name = String -- Name of cards/countries/leaders
type Row = Int -- Rows that the card can be played on/affects
type Damage = Int
type Deck = (Country, [Card]) -- (Country name, relevant cards)
