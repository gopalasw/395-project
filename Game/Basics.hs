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

removeCard :: Int -> [Card] -> [Card]
removeCard _ []     = []
removeCard 0 (x:xs) = xs
removeCard i (x:xs) = x : (removeCard (i - 1) xs)


getTotalDamage :: [Card] -> Int
getTotalDamage ls =
   foldl (+) 0 (map getDamage ls)

getDamage :: Card -> Int
getDamage (CUnit _ _ _ d) = d
getDamage _               = 0

