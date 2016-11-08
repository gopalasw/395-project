module Game.Basics where

import Grammar.Grammar
import Cards.Cards
import Data.List
import System.Random
import Control.Exception
import System.IO.Unsafe
import Text.Read
import Control.Applicative

main = undefined

-- Daw a certain number of card from a country
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

-- Draw a card from a deck, 
-- return the card and the rest of the deck
drawCard :: [Card] -> (Maybe Card, [Card])
drawCard = undefined

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
removeCardIndex i (x:xs) = x : (removeCard (i - 1) xs)

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