module Game.Init where

import Grammar.Grammar
import Cards.Cards
import Game.Basics
import Data.List

init :: (Country, Country) -> (Card, Card) -> Board
init (c1, c2) (l1, l2) = Board
  {
    a          = genPlayer (getCards c1) c1 l1,
    b          = genPlayer (getCards c2) c2 l2,
    weather    = [],
    roundScore = (0, 0),
    isATurn    = True
  }
  where
    getCards :: Country -> ([Card], [Card])
    getCards c =
      case drawCards c 10 northernDeck of
        Just res -> res
        Nothing  -> ([], [])

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


swapOneCard :: Player -> Card -> Player
swapOneCard p c =
  case drawCard (cardsLeft p) of
    (Just c', ls) -> p { cardsInHand = c' : (delete c (cardsInHand p)),
                         cardsLeft   = c  : ls }

