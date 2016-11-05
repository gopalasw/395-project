module Game.Basics where

import Grammar.Grammar
--import Data.Random.Extras
import Game.Cards
import Data.List
import System.Random
import Control.Exception

main = undefined

-- players choose country, leader, deck is initialised 
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
      case drawCards c 10 allCards of
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
drawCard = undefined


getCard :: Int -> [Card] -> Maybe Card
getCard _ []     = Nothing
getCard 0 (x:xs) = Just x
getCard i (x:xs) = getCard (i - 1) xs

removeCard :: Int -> [Card] -> [Card]
removeCard _ []     = []
removeCard 0 (x:xs) = xs
removeCard i (x:xs) = x : (removeCard (i - 1) xs)


-- In the begining of a game
swapOneCard :: Player -> Card -> Player 
swapOneCard p c = 
  case drawCard (cardsLeft p) of
    (Just c', ls) -> p { cardsInHand = c' : (delete c (cardsInHand p)),
                         cardsLeft   = c  : ls }

discardOpCard :: Card -> Player -> Player
discardOpCard c p =
  p { cardsInHand = delete c (cardsInHand p),
      usedCards   = c : (usedCards p) }
  


-- player chooses card to play or pass, board updates
playTurn :: Board -> Board
playTurn currentB 
  | isATurn currentB = currentB { a = (playCard (a currentB)) }
  | otherwise        = currentB { b = (playCard (b currentB)) }



-- !!!!! This function does not work !!!!! --
-- play a card, but the problem is we cannot get Card out from IO Monad
playCard :: Player -> Player
playCard p = 
  p { cardsOnBoard = cardPlayed : (cardsOnBoard p),
      cardsInHand  = delete cardPlayed (cardsInHand p)}
  where 
    cardPlayed :: Card
    cardPlayed = do 
      res <- getAction (cardsInHand p)
      return res


-- Takes input from the terminal
getAction :: [Card] -> IO Card
getAction cs = do 
  putStrLn $ show $ zip [1 .. (length cs)] (map (getName) cs)
  putStrLn "Which card do you want to play?"
  ans <- getLine
  res <- try (read ans :: Int)
  case res of
    Left err -> do
      putStrLn "Exception caught" ++ show err
      getAction cs
    Right res' -> head (drop (res' - 1) cs)


getName :: Card -> String
getName (CWeather n _)  = n
getName (CUnit n _ _ _) = n
getName (CLeader l)     = show l
getName CPass           = "Pass"
  


--        Previous board, isATurn
roundStart :: Board -> Bool -> Board 
roundStart b@(Board p1 p2 _ _ _) bool =
  b { a = p1 { usedCards = (cardsOnBoard p1) ++ (usedCards p1),
               cardsOnBoard = []},
      b = p2 { usedCards = (cardsOnBoard p2) ++ (usedCards p2),
               cardsOnBoard = []},
      weather = [],
      roundScore   = (0, 0),
      isATurn   = bool}



-- evaluates current state of board, updates round scores
evaluateTurn :: Board -> Board
evaluateTurn currentB@(Board p1 p2 _ _ pTurn) =
  currentB { 
    roundScore = (totalDamage p1, totalDamage p2),
    isATurn    = not pTurn
  }
  where
    totalDamage p = getTotalDamage (cardsOnBoard p)
-- TODO
-- This is a minimal version of evaluate, we only cares about damage for now

-- evaluates current state of board, updates player scores 
evaluateRound :: Board -> Board 
evaluateRound b@(Board p1 p2 _ (s1, s2) _) 
  | s1 < s2  = b { a = p1 { score = 0 : (score p1)},
                   b = p2 { score = 1 : (score p2)}} 
  | s1 > s2  = b { a = p1 { score = 1 : (score p1)},
                   b = p2 { score = 0 : (score p2)}}
  | s1 == s2 = b { a = p1 { score = 0 : (score p1)},
                   b = p2 { score = 0 : (score p2)}} 

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



-- TODO 
-- It would be nice if we had a lives field in Player data structure,
-- subtract one everytime the player lose or draw
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




