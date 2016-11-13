module Game.Init where

import Grammar.Grammar
import Grammar.Board
import Cards.Cards
import Game.Basics

import Data.List
import System.Random

initBoard :: StdGen -> (Country, Country) -> (Card, Card) -> Board
initBoard gen (c1, c2) (l1, l2) = Board
  {
    a          = genPlayer (getCards gen c1) c1 l1,
    b          = genPlayer (getCards nGen c2) c2 l2,
    weather    = [],
    roundScore = (0, 0),
    isATurn    = True,
    randomSeed = boardGen
  }
    where
      (_, nGen) = next gen
      (_, boardGen) = next nGen


getCards :: StdGen -> Country -> ([Card], [Card])
getCards seed c
  | c == Northern =
    case drawCardsR seed 10 northernCards of
      (Just drew, left) -> (drew, []) -- left)
      (Nothing,   left) -> ([],   []) --left)
  | c == Nilfgaard =
    case drawCardsR seed 10 nilfgaardCards  of
      (Just drew, left) -> (drew, []) --left)
      (Nothing,   left) -> ([],   []) --left)
  | otherwise       =  ([],[])


genPlayer :: ([Card], [Card]) -> Country -> Card -> Player
genPlayer (drew, left) c l =
  Player {
    cardsInHand  = drew,
    cardsLeft    = left,
    cardsOnBoard = [],
    usedCards    = [],
    score        = [],
    leader       = l,
    country      = c }


swapOneCard :: Player -> StdGen -> Card -> Player
swapOneCard p@(Player hand left _ _ _ _ country) seed c
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

