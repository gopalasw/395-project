module Game.Basics where

import Grammar.Grammar

main = undefined
--
init :: (Country, Country) -> (Card, Card) -> Board 
init (c1, c2) (l1, l2) = Board
  {
    a          = genPlayer (drawCard c1) c1 l1,    
    b          = genPlayer (drawCard c2) c2 l2,
    weather    = [],
    roundScore = (0, 0),
    isATurn    = True
  }

genPlayer :: ([Card], [Card]) -> Country -> Card -> Player
genPlayer (drew, left) c l =
  Player { 
    cardsInHand  = drew,
    cardsLeft    = left,
    cardsOnBoard = [],
    usedCards    = [],
    score        = 0,
    leader       = l,
    country      = c }


-- fst: cards drew; snd: cards left
drawCard :: Country ->  ([Card], [Card])
drawCard = undefined

swapCard :: Board -> Board 
swapCard = undefined

turn :: Board -> Board
turn = undefined

getAction :: Player -> Player
getAction = undefined

evaluate :: Board -> Board
evaluate = undefined

roundOver :: Board -> Bool
roundOver (Board p1 p2 _ _ _) =
  (isPass p1) && (isPass p2)
  where 
    isPass :: Player -> Bool
    isPass p = (head $ cardsOnBoard $ p) == CPass
        
  


gameOver :: Board -> Bool
gameOver = undefined 
