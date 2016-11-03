module Game.Basics where

import Grammar.Grammar
--import Data.Random.Extras
import Game.Cards
import Data.List
import System.Random

main = undefined

-- players choose country, leader, deck is initialised 
init :: (Country, Country) -> (Card, Card) -> Board 
init (c1, c2) (l1, l2) = Board
  {
    a          = genPlayer (drawCards c1 10 allCards) c1 l1,    
    b          = genPlayer (drawCards c2) c2 l2,
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

type cardsToDraw = Int

drawCards :: Country -> Int -> [Card] -> Maybe ([Card], [Card])
drawCards _ _ [] = Nothing
drawCards _ 0 ls = Just ([], ls)
drawCards c i ls = 
  case tuple of
  (Just card, remains) -> 
    case (drawCards c (i - 1) remains) of
      Just (dealt, left) -> Just (card:dealt, left)
      Nothing            -> Nothing
  otherwise            -> Nothing
  where
    tuple = drawCard ls 

drawCard :: [Card] -> (Maybe Card, [Card])
drawCard cards = do 
  ran :: Int  <- randomRIO (0, length cards) :: IO Int
  (getCard ran cards, removeCard ran cards)

getCard :: Int -> [Card] -> Maybe Card
getCard _ []     = Nothing
getCard 0 (x:xs) = Just x
getCard i (x:xs) = getCard (i - 1) xs

removeCard :: Int -> [Card] -> [Card]
removeCard _ []     = []
removeCard 0 (x:xs) = xs
removeCard i (x:xs) = x : (removeCard (i - 1) xs)

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
    totalDamage p = getTotalDamage (cardsOnBoard p)
-- TODO
-- This is a minimal version of evaluate, we only cares about damage for now

getTotalDamage :: [Card] -> Int 
getTotalDamage ls = 
   foldl (+) 0 (map getDamage ls)

getDamage :: Card -> Int
getDamage (CUnit _ _ _ d) = d 
getDamage _               = 0

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
  ((fst lengths == 2) && (fst scores == 0)) ||
  ((snd lengths == 2) && (snd scores == 0)) ||
  fst scores == 2 ||
  snd scores == 2 || 
  (((fst lengths) == (snd lengths)) && 
  (fst lengths == 3))
  where
    lengths :: (Int, Int)
    lengths = ((length $ score $ p1), (length $ score $ p2)) 
    scores  :: (Int, Int)
    scores  = ((foldl (+) 0 (score p1)), (foldl (+) 0 (score p2)))




