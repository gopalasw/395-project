module Game.Basics where

import Grammar.Grammar
import Grammar.Board
import Cards.Cards
import Cards.NeutralCards

import System.Random
import Data.List
import Data.Maybe

type CardsInDeck = [Card]
type CardsDrawn  = [Card]
type CardsLeft   = [Card]
type CardsToDraw = Int

-- draws n random cards
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

-- calculate total damage, taking weather into account
getTotalDamage :: [Card] -> Weather -> Int
getTotalDamage ls weather =
   foldl (+) 0 (map (getDamage (Just weather)) ls)

-- get the damage of a single card, taking weather into account
getDamage :: (Maybe Weather) -> Card -> Int
getDamage (Just weather) (CUnit _ r _ d) =
  if hasWeather r weather then 1 else d
getDamage Nothing (CUnit _ _ _ d) = d
getDamage _ _ = 0

-- find if the row is affected by weather
hasWeather :: Row -> Weather -> Bool
hasWeather 1 (True, _, _) = True
hasWeather 2 (_, True, _) = True
hasWeather 3 (_, _, True) = True
hasWeather _ _ = False

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
