module Game.Turn where

import Control.Applicative
import Data.List
import System.Random
import Text.Read

import Grammar.Grammar
import Grammar.Board
import Grammar.PrettyPrint
import Game.Basics
import Game.AbilityEffects
import Cards.Cards
import Game.UserInput

-- sequence of actions in a turn
turnLoop :: IO Board -> IO Board
turnLoop board = do
  b <- playTurn board
  b <- pure $ evaluateTurn b
  putStrLn $ "\n" ++  prettyPrintBoard b
  if not (roundOver b)
  then turnLoop (pure b)
  else putStrLn "\n----------Round over----------\n\n" >> return b

-- check to see if round over
roundOver :: Board -> Bool
roundOver (Board p1 p2 _ _ _ _) =
  if (noCardsPlayed p1) || (noCardsPlayed p2) then False else (lastIsPass p1) && (lastIsPass p2)

-- checks if other player has passed
otherPlayerPassed :: Board -> Bool
otherPlayerPassed (Board p1 p2 _ _ pTurn _) = if (noCardsPlayed otherP) then False else lastIsPass otherP
  where otherP = if pTurn then p2 else p1

-- checks if there are no cards on board
noCardsPlayed :: Player -> Bool
noCardsPlayed p = [] == cardsOnBoard p

-- checks if the last card played was a pass
lastIsPass :: Player -> Bool
lastIsPass p = (head $ cardsOnBoard $ p) == CPass

-- updates turn score, updates whose turn it is
evaluateTurn :: Board -> Board
evaluateTurn currentB@(Board p1 p2 w _ pTurn _) =
  cardsAbilityDamage $ currentB {
    roundScore = (totalDamage p1, totalDamage p2),
    isATurn    = if otherPlayerPassed currentB then pTurn else not pTurn
  }
  where
    totalDamage p = getTotalDamage (cardsOnBoard p) w

-- swaps whose turn it is
swapWhoseTurn :: Board -> Board
swapWhoseTurn currentB@(Board _ _ _ _ pTurn _) =
  currentB {
    isATurn = not pTurn
  }


updateRandomSeed :: Board -> Board
updateRandomSeed b = b { randomSeed = g }
  where
    seed   = randomSeed b
    (i, g) = next seed

-- goes through sequence of actions for playing a turn
playTurn :: IO Board -> IO Board
playTurn board = do
  board' <- board
  if isAI board' then do
    return $  snd $ evaluateAB board'
  else do
    card <- getCardHelper (getCurHand board') getPlayIndex
    board' <- pure $ updateRandomSeed board'
    evalAbility (updateCurPlayer (updateWeather board' card)
                               (updatePlayedCard card)) card
  where
    getCurHand board' =
      if isATurn board' then
        (cardsInHand $ a board')
      else
        (cardsInHand $ b board')
    curLeader board' =
      if isATurn board' then
        leaderHelper (leader $ a board')
      else leaderHelper (leader $ b board')
    leaderHelper (leaderCard, b) =
      if b then [] else [leaderCard]
    isAI board' = if isATurn board' then (isComp $ a board') else isComp $ b board'

-- for the AI to get a card
getCardAI :: [Card] -> StdGen-> IO Card -- TODO: add a heuristic
getCardAI d g = do
  pure $ head s
  where
    (idx, _) = randomR (0, length d - 1) g
    (f, s)   = splitAt idx d

-- updates the played card from hand to board
updatePlayedCard :: Card -> Player -> Player
updatePlayedCard CPass p =
  p { cardsOnBoard = CPass : (cardsOnBoard p),
      cardsInHand  = cardsInHand p }
updatePlayedCard (c@(CWeather _ _)) p =
  p { cardsInHand  = delete c (cardsInHand p) }
updatePlayedCard (CLeader _) p = p
updatePlayedCard card p =
  p { cardsOnBoard = card : (cardsOnBoard p),
      cardsInHand  = delete card (cardsInHand p)}


--------------------------------- MIN MAX -------------------------

emptyBoard = Board
  {
    a          = emptyPlayer,
    b          = emptyPlayer,
    weather    = (False, False, False),
    roundScore = (0, 0),
    isATurn    = True,
    randomSeed = mkStdGen 0
  }

emptyPlayer = Player
  {
    cardsInHand = [],
    cardsLeft = [],
    cardsOnBoard = [],
    usedCards = [],
    lives = [],
    leader = (CPass, False),
    country = Northern,
    isComp = False
  }


data Tree a = Node a [Tree a] deriving (Show)

-- Evaluation functions
moves :: Board -> [Board]
moves board =
  boardMoves board (cardsInHand curPlayer)
  where curPlayer = if isATurn board then a board else b board

boardMoves :: Board -> [Card] -> [Board]
boardMoves board [] = []
boardMoves board (card:xs) =
  evaluateTurn (updateCurPlayer (updateWeather board card)
                  (updatePlayedCard card)) : (boardMoves board xs)

static :: Board -> Int
static board =
  case compareScore aScore bScore of
    Lt -> 1
    Eq -> 1
    Gt -> -1
  where
    aScore = fst $ roundScore board
    bScore = snd $ roundScore board

compareScore :: Int -> Int -> Cmp
compareScore a b =
  if a > b then Gt
    else
      if a < b then Lt
        else Eq

data Cmp = Lt | Gt | Eq

------ Tree Functions -------
maptree :: (a -> b) -> Tree a -> Tree (b, a)
maptree f (Node a []) = Node ((f a), a) []
maptree f (Node a subtrees) = Node ((f a), a) (map (maptree f) subtrees)

reptree :: (a -> [a]) -> a -> Tree a
reptree f a = Node a (map (reptree f) (f a))

gametree :: Board -> Tree Board
gametree p = reptree moves p
------------------------------------------------------------------------------
-- Alpha-Beta Pruning

maximumPair :: Ord a => [(a, Board)] -> (a, Board)
maximumPair (p:nums) = maximumPair' p nums
  where
    maximumPair' maxSoFar [] = maxSoFar
    maximumPair' maxSoFar (x:xs) =
      case (fst maxSoFar >= fst x) of
        True -> maximumPair' maxSoFar xs
        False -> maximumPair' x xs    

minimumPair :: Ord a => [(a, Board)] -> (a, Board)
minimumPair (p:nums) = minimumPair' p nums
  where
    minimumPair' minSoFar [] = minSoFar
    minimumPair' minSoFar (x:xs) =
      case (fst minSoFar <= fst x) of
        True -> minimumPair' minSoFar xs
        False -> minimumPair' x xs

maximizeAB :: Ord a => Tree (a, Board) -> (a, Board)
maximizeAB (Node n []) = n
maximizeAB t = maximumPair $ maximize' t

minimizeAB :: Ord a => Tree (a, Board) -> (a, Board)
minimizeAB (Node n []) = n
minimizeAB t = minimumPair $ minimize' t

maximize' :: Ord a => Tree (a, Board) -> [(a, Board)]
maximize' (Node n []) = [n]
maximize' (Node n l) = mapmin (map minimize' l)

minimize' :: Ord a => Tree (a, Board) -> [(a, Board)]
minimize' (Node n []) = [n]
minimize' (Node n l) = mapmax (map maximize' l)

mapmin :: Ord a => [[(a, Board)]] -> [(a, Board)]
mapmin (nums:rest) = minPair : (omitmin minPair rest)
  where minPair = minimumPair nums

mapmax :: Ord a => [[(a, Board)]] -> [(a, Board)]
mapmax (nums:rest) = maxPair : (omitmax maxPair rest)
  where maxPair = maximumPair nums

omitmin :: Ord a => (a, Board) -> [[(a, Board)]] -> [(a, Board)]
omitmin pot [] = []
omitmin pot (nums:rest) =
  if minleq nums pot then
    omitmin pot rest
  else
    minPair : (omitmin minPair rest)
    where minPair = minimumPair nums

omitmax :: Ord a => (a, Board) -> [[(a, Board)]] -> [(a, Board)]
omitmax pot [] = []
omitmax pot (nums:rest) =
  if maxgeq nums pot then
    omitmax pot rest
  else
    maxPair : (omitmax maxPair rest)
    where maxPair = maximumPair nums

minleq :: Ord a => [(a, b)] -> (a, Board) -> Bool
minleq [] pot = False
minleq (n:rest) pot =
  if (fst n) <= (fst pot) then
    True
  else
    minleq rest pot

maxgeq :: Ord a => [(a, b)] -> (a, Board) -> Bool
maxgeq [] pot = False
maxgeq (n:rest) pot =
  if (fst n) >= (fst pot) then
    True
  else
    maxgeq rest pot

highfirst :: Ord a => Tree (a, b) -> Tree (a, b)
highfirst (Node n sub) =
  Node n (quicksort (lower) (map lowfirst sub))

lowfirst :: Ord a => Tree (a, b) -> Tree (a, b)
lowfirst (Node n sub) =
  Node n (quicksort (higher) (map highfirst sub))

higher :: Ord a => Tree (a, b) -> Tree (a, b) -> Bool
higher (Node n1 sub1) (Node n2 sub2) = (fst n1) > (fst n2)

lower :: Ord a => Tree (a, b) -> Tree (a, b) -> Bool
lower (Node n1 sub1) (Node n2 sub2) = (fst n1) <= (fst n2)

quicksort :: (Ord a) => (Tree (a, b) -> Tree (a, b) -> Bool) -> [Tree (a, b)]-> [Tree (a, b)]
quicksort f [] = []
quicksort f (x:xs) =
    let smallerSorted = quicksort f [a | a <- xs, f a x]
        biggerSorted = quicksort f [a | a <- xs, (not (f a x))]
    in  smallerSorted ++ [x] ++ biggerSorted

prune :: Int -> Tree a -> Tree a
prune 0 (Node a x) = Node a []
prune n (Node a x) = Node a (map (prune (n - 1)) x)

evalfuncAB :: Ord a => Board -> (Tree (a, Board) -> (a, Board))
evalfuncAB board = if not $ isATurn board then maximizeAB else minimizeAB

evaluateAB :: Board -> (Int, Board)
evaluateAB board = (evalfuncAB board) . highfirst . maptree static . prune 1 . gametree $ board
