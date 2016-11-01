module Game.Basics where

import Grammar.Grammar
--
init :: (Country, Country) -> (Card, Card) -> Board 
init (c1, c2) (l1, l2) = Board
  {
    a          = genPlayer (drawCard c1) c1 l1    
    b          = genPlayer (drawCard c2) c2 l2
    weather    = []
    roundScore = (0, 0)
  }

genPlayer :: ([Card], [Card]) -> Country -> Card -> Player
genPlayer (drew, left) c l =
  Player { 
    cardsInHand = drew
    cardsLeft   = left
    usedCards   = []
    score       = []
    leader      = l
    country     = c }


-- fst: cards drew; snd: cards left
drawCard :: Country ->  ([Card], [Card])
drawCard = undefined
