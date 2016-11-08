module Game.Basics where

import Grammar.Grammar
import Cards.Cards
import Cards.NeutralCards
import System.Random
import Data.List
import Data.Maybe

type CardsInDeck = [Card]
type CardsDrawn  = [Card]
type CardsLeft   = [Card]
type CardsToDraw = Int

drawCardsR :: StdGen -> CardsToDraw -> CardsInDeck -> (Maybe CardsDrawn, CardsLeft)
drawCardsR _ _ [] = (Nothing, [])
drawCardsR _ 0 l  = (Nothing, l)
drawCardsR g n l  = (Just res, filter (not . p) l)
  where
    res = catMaybes $ helper g n l
    p = \x -> x `elem` res
    helper _ _ [] = []
    helper _ 0 _  = []
    helper x y ls  = c : (helper x (y-1) rst)
      where (c, rst) = drawCardR x ls

-- TODO: Test drawCardsR and drawCardR

-- randomly draw a card from a deck
drawCardR :: StdGen -> [Card] -> (Maybe Card, [Card])
drawCardR _ [] = (Nothing, [])
drawCardR g d  = (Just $ head s, f ++ (tail s))
 where
   (idx, _) = randomR (0, length d - 1) g
   (f, s)   = splitAt idx d

-- Get a card by its index
-- Helper function for drawCard
getCard :: Int -> [Card] -> Maybe Card
getCard _ []     = Nothing
getCard 0 (x:xs) = Just x
getCard i (x:xs) = getCard (i - 1) xs

-- Remove a card from a deck by its index
removeCardIndex :: Int -> [Card] -> [Card]
removeCardIndex _ []     = []
removeCardIndex 0 (x:xs) = xs
removeCardIndex i ls     = fst ++ (tail snd)
  where (fst, snd) = splitAt i ls

removeCardIndexTest =
  (removeCardIndex 0 neutralCards) == (tail neutralCards) &&
  (removeCardIndex 3 deck == [geralt, cirilla, vesemir, triss])
  where deck = (take 5 neutralCards)

-- Removes a card from a deck
removeCard :: Card -> [Card] -> [Card]
removeCard _ [] = []
removeCard card (c:cs) =
  if card == c then
    cs
  else
    c : (removeCard card cs)

getTotalDamage :: [Card] -> Int
getTotalDamage ls =
   foldl (+) 0 (map getDamage ls)

getDamage :: Card -> Int
getDamage (CUnit _ _ _ d) = d
getDamage _               = 0

-- updates current player on the board with a player update function
updateCurPlayer :: Board -> (Player -> Player) -> Board
updateCurPlayer board func =
  if (isATurn board) then
    board { a = func (a board) }
  else
    board { b = func (b board) }

-- updates opponent player on the board with a player update function
updateOppPlayer :: Board -> (Player -> Player) -> Board
updateOppPlayer board func =
  if (isATurn board) then
    board { b = func (b board) }
  else
    board { a = func (a board) }
