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

turnLoop :: IO Board -> IO Board
turnLoop board = do
  b <- playTurn board
  b <- pure $ evaluateTurn b
  putStrLn $ "\n" ++  prettyPrintBoard b
  if not (roundOver b)
  then turnLoop (pure b)
  else putStrLn "\n----------Round over----------\n\n" >> return b


roundOver :: Board -> Bool
roundOver (Board p1 p2 _ _ _ _) =
  if (noCardsPlayed p1) || (noCardsPlayed p2) then False else (lastIsPass p1) && (lastIsPass p2)


otherPlayerPassed :: Board -> Bool
otherPlayerPassed (Board p1 p2 _ _ pTurn _) = if (noCardsPlayed otherP) then False else lastIsPass otherP
  where otherP = if pTurn then p2 else p1


noCardsPlayed :: Player -> Bool
noCardsPlayed p = [] == cardsOnBoard p


lastIsPass :: Player -> Bool
lastIsPass p = (head $ cardsOnBoard $ p) == CPass


evaluateTurn :: Board -> Board
evaluateTurn currentB@(Board p1 p2 w _ pTurn _) =
  cardsAbilityDamage $ currentB {
    roundScore = (totalDamage p1, totalDamage p2),
    isATurn    = if otherPlayerPassed currentB then pTurn else not pTurn
  }
  where
    totalDamage p = getTotalDamage (cardsOnBoard p) w


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


playTurn :: IO Board -> IO Board
playTurn board = do
  board' <- board
  if isAI board' then do
    return $ snd $ evaluateAB board'
  else do
    card <- getCardHelper (curHand board') getPlayIndex
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
    curHand board' = (getCurHand board') ++ (curLeader board') ++ [CPass]


getCardAI :: [Card] -> StdGen-> IO Card -- TODO: add a heuristic
getCardAI d g = do
  pure $ head s
  where
    (idx, _) = randomR (0, length d - 1) g
    (f, s)   = splitAt idx d


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
  if isATurn board then
    case compareScore aScore bScore of
      Lt -> -1
      Eq -> 0
      Gt -> 1
  else 
    case compareScore aScore bScore of
      Lt -> 1
      Eq -> 0
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
maximumPair nums = (val, board)
  where
    val = maximum (map fst nums)
    board = case lookup val nums of
      Just b -> b
      Nothing -> emptyBoard

minimumPair :: Ord a => [(a, Board)] -> (a, Board)
minimumPair nums = (val, board)
  where
    val = minimum (map fst nums)
    board = case lookup val nums of
      Just b -> b
      Nothing -> emptyBoard

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
mapmin (nums:rest) = minPair : (omitmin (fst minPair) rest) 
  where minPair = minimumPair nums

mapmax :: Ord a => [[(a, Board)]] -> [(a, Board)]
mapmax (nums:rest) = maxPair : (omitmax (fst maxPair) rest)
  where maxPair = maximumPair nums

omitmin :: Ord a => a -> [[(a, Board)]] -> [(a, Board)]
omitmin pot [] = []
omitmin pot (nums:rest) =
  if minleq nums pot then
    omitmin pot rest
  else
    (val, board) : (omitmin val rest)
    where
      val = minimum (map fst nums)
      board = case lookup val nums of
        Just b -> b
        Nothing -> emptyBoard

omitmax :: Ord a => a -> [[(a, Board)]] -> [(a, Board)]
omitmax pot [] = []
omitmax pot (nums:rest) =
  if maxgeq nums pot then
    omitmax pot rest
  else
    (val, board) : (omitmax val rest)
    where
      val = maximum (map fst nums)
      board = case lookup val nums of
        Just b -> b
        Nothing -> emptyBoard


minleq :: Ord a => [(a, b)] -> a -> Bool
minleq [] pot = False
minleq (n:rest) pot =
  if (fst n) <= pot then
    True
  else
    minleq rest pot

maxgeq :: Ord a => [(a, b)] -> a -> Bool
maxgeq [] pot = False
maxgeq (n:rest) pot =
  if (fst n) >= pot then
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
evalfuncAB board = if isATurn board then maximizeAB else minimizeAB

evaluateAB :: Board -> (Int, Board)
evaluateAB board = (evalfuncAB board) . highfirst . maptree static . prune 5 . gametree $ board

{-
example :: IO Board
example = do 
  t <- getCurrentTime
  return $ initVersusAIBoard (seed t) (Northern, Northern) ((CLeader Relentless), (CLeader NorthCommander))
  where seed t = mkStdGen $ floor $ utctDayTime t

  -}