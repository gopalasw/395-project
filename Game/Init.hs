module Game.Init where

import Grammar.Grammar
import Grammar.Board
import Cards.Cards
import Game.Basics

import Data.List
import System.Random

-- initializes board with 2 human players
initBoard :: StdGen -> (Country, Country) -> (Card, Card) -> Board
initBoard gen (c1, c2) (l1, l2) = Board
  {
    a          = genPlayer (addL (getCards gen c1) l1)  c1 l1,
    b          = genPlayer (addL (getCards nGen c2) l2) c2 l2,
    weather    = (False, False, False),
    roundScore = (0, 0),
    isATurn    = True,
    randomSeed = boardGen
  }
    where
      (_, nGen) = next gen
      (_, boardGen) = next nGen

-- initializes board with ai player and human player
initVersusAIBoard :: StdGen -> (Country, Country) -> (Card, Card) -> Board
initVersusAIBoard gen (c1, c2) (l1, l2) = Board
  {
    a          = genPlayer (addL (getCards gen c1) l1)  c1 l1,
    b          = genAI     (addL (getCards nGen c2) l2) c2 l2,
    weather    = (False, False, False),
    roundScore = (0, 0),
    isATurn    = True,
    randomSeed = boardGen
  }
    where
      (_, nGen) = next gen
      (_, boardGen) = next nGen

-- adds leader to hand
addL :: ([Card], [Card]) -> Card -> ([Card], [Card])
addL (f, s) l = (l:f, s)

-- gets cards from a specified country deck
getCards :: StdGen -> Country -> ([Card], [Card])
getCards seed c
  | c == Northern =
    case drawCardsR seed 10 northernCards of
      (Just drew, left) -> (drew, []) -- left)
      (Nothing,   left) -> ([],   []) -- left)
  | c == Nilfgaard =
    case drawCardsR seed 10 nilfgaardCards  of
      (Just drew, left) -> (drew, []) --left)
      (Nothing,   left) -> ([],   []) --left)
  | otherwise       =  ([],[])

-- generates player with given information
genPlayer :: ([Card], [Card]) -> Country -> Card -> Player
genPlayer (drew, left) c l =
  Player {
    cardsInHand  = drew,
    cardsLeft    = left,
    cardsOnBoard = [],
    usedCards    = [],
    lives        = [],
    leader       = (l, False),
    country      = c,
    isComp       = False }

-- generates AI player
genAI :: ([Card], [Card]) -> Country -> Card -> Player
genAI (drew, left) c l =
  Player {
    cardsInHand  = drew,
    cardsLeft    = left,
    cardsOnBoard = [],
    usedCards    = [],
    lives        = [],
    leader       = (l, False),
    country      = c,
    isComp       = True }

-- helper function for swapping cards
swapOneCard :: Player -> StdGen -> Card -> Player
swapOneCard p@(Player hand left _ _ _ _ country _) seed c
  | country == Northern  =
    case drawCardR seed northernCards of
      (Just c', ls) -> p { cardsInHand = c' : (delete c (cardsInHand p)),
                           cardsLeft   = c  : ls }
      (Nothing,  _) -> p
  | country == Nilfgaard =
    case drawCardR seed nilfgaardCards of
      (Just c', ls) -> p { cardsInHand = c' : (delete c (cardsInHand p)),
                           cardsLeft   = c  : ls }
      (Nothing,  _) -> p
  | otherwise            = p

