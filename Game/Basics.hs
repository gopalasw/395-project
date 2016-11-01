module Game.Basics where

import Grammar.Grammar

main = undefined

-- players choose country, leader, deck is initialised 
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
    score        = [],
    leader       = l,
    country      = c }


-- fst: cards drew; snd: cards left
drawCard :: Country ->  ([Card], [Card])
drawCard = undefined

swapCard :: Board -> Board 
swapCard = undefined

-- player chooses card to play or pass, board updates
playTurn :: Board -> Board
playTurn currentB 
  | isATurn currentB = currentB { a = (playCard (a currentB)) }
  | otherwise        = currentB { b = (playCard (b currentB)) }

playCard :: Player -> Player
playCard = undefined

-- evaluates current state of board, updates round scores
evaluate :: Board -> Board
evaluate currentB@(Board p1 p2 _ _ pTurn) =
  currentB { 
    roundScore = (totalDamage p1, totalDamage p2),
    isATurn    = not pTurn
  }
  where
    totalDamage p = foldl (+) 0 $  map damage $ getUnits (cardsOnBoard p)
-- TODO
-- This is a minimal version of evaluate, we only cares about damage for now

getUnits :: [Card] -> [Unit]
getUnits ls = 
   (filter (\x -> x == CUnit) ls)
-- TODO
-- Finsih getUnits. It filters throught a list of cardOnBoard and returns 
-- all CUnit Cards.


-- checks if round is over by checking if both players have passed
roundOver :: Board -> Bool
roundOver (Board p1 p2 _ _ _) =
  (isPass p1) && (isPass p2)
  where 
    isPass :: Player -> Bool
    isPass p = (head $ cardsOnBoard $ p) == CPass        

-- checks if game is over by checking if player a or b has won
gameOver :: Board -> Bool
gameOver (Board p1 p2 _ _ _) =
  fst scores == 2 ||
  snd scores == 2 || 
  ((fst lengths) == (snd lengths)) && 
  (fst lengths == 3)
  where
    lengths :: (Int, Int)
    lengths = ((length $ score $ p1), (length $ score $ p2)) 
    scores  :: (Int, Int)
    scores  = ((foldl (+) 0 (score p1)), (foldl (+) 0 (score p2)))




