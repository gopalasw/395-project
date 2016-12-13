module MinMax where
import Data.List
import Grammar.Grammar
import Grammar.Board
import Game.Turn
import Game.Basics
import Game.Init
import Game.Round
import Grammar.PrettyPrint
import Grammar.Board
import Control.Applicative
import System.Random
import Data.Time.Clock

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
maptree :: (a -> b) -> Tree a -> Tree b
maptree f (Node a []) = Node (f a) []
maptree f (Node a subtrees) = Node (f a) (map (maptree f) subtrees)

foldtree :: (a -> b -> b) -> (b -> b -> b) -> b -> Tree a -> b
foldtree f g a (Node label subtrees) =
  f label (foldtree' f g a subtrees)

foldtree' :: (a -> b -> b) -> (b -> b -> b) -> b -> [Tree a] -> b
foldtree' f g a [] = a
foldtree' f g a (subtree:rest) =
  g (foldtree f g a subtree) (foldtree' f g a rest)

reptree :: (a -> [a]) -> a -> Tree a
reptree f a = Node a (map (reptree f) (f a))

gametree :: Board -> Tree Board
gametree p = reptree moves p
------------------------------------------------------------------------------
-- Alpha-Beta Pruning

maximizeAB :: Ord a => Tree a -> a 
maximizeAB (Node n []) = n
maximizeAB t = maximum $ maximize' t

minimizeAB :: Ord a => Tree a -> a 
minimizeAB (Node n []) = n
minimizeAB t = minimum $ minimize' t

maximize' :: Ord a => Tree a -> [a]
maximize' (Node n []) = [n]
maximize' (Node n l) = mapmin (map minimize' l)

minimize' :: Ord a => Tree a -> [a]
minimize' (Node n []) = [n]
minimize' (Node n l) = mapmax (map maximize' l)

mapmin :: Ord a => [[a]] -> [a]
mapmin (nums:rest) = (minimum nums) : (omitmin (minimum nums) rest)

mapmax :: Ord a => [[a]] -> [a]
mapmax (nums:rest) = (maximum nums : omitmax (maximum nums) rest)

omitmin :: Ord a => a -> [[a]] -> [a]
omitmin pot [] = []
omitmin pot (nums:rest) =
  if minleq nums pot then
    omitmin pot rest
  else
    (minimum nums) : (omitmin (minimum nums) rest)

omitmax :: Ord a => a -> [[a]] -> [a]
omitmax pot [] = []
omitmax pot (nums:rest) =
  if maxgeq nums pot then
    omitmax pot rest
  else
    (maximum nums) : (omitmax (maximum nums) rest)

minleq :: Ord a => [a] -> a -> Bool
minleq [] pot = False
minleq (n:rest) pot =
  if n <= pot then
    True
  else
    minleq rest pot

maxgeq :: Ord a => [a] -> a -> Bool
maxgeq [] pot = False
maxgeq (n:rest) pot =
  if n >= pot then
    True
  else
    maxgeq rest pot

highfirst :: Ord a => Tree a -> Tree a
highfirst (Node n sub) =
  Node n (quicksort (lower) (map lowfirst sub))

lowfirst :: Ord a => Tree a -> Tree a
lowfirst (Node n sub) =
  Node n (quicksort (higher) (map highfirst sub))

higher :: Ord a => Tree a -> Tree a -> Bool
higher (Node n1 sub1) (Node n2 sub2) = n1 > n2

lower :: Ord a => Tree a -> Tree a -> Bool
lower (Node n1 sub1) (Node n2 sub2) = n1 <= n2

quicksort :: (Ord a) => (Tree a -> Tree a -> Bool) -> [Tree a]-> [Tree a] 
quicksort f [] = []  
quicksort f (x:xs) =   
    let smallerSorted = quicksort f [a | a <- xs, f a x]  
        biggerSorted = quicksort f [a | a <- xs, (not (f a x))]  
    in  smallerSorted ++ [x] ++ biggerSorted  

prune :: Int -> Tree a -> Tree a
prune 0 (Node a x) = Node a []
prune n (Node a x) = Node a (map (prune (n - 1)) x)

evalfuncAB :: Ord a => Board -> (Tree a -> a)
evalfuncAB board = if isATurn board then maximizeAB else minimizeAB

evaluateAB :: Board -> Int
evaluateAB board = (evalfuncAB board) . highfirst . maptree static . prune 4 . gametree $ board

example :: IO Board
example = do 
  t <- getCurrentTime
  return $ initVersusAIBoard (seed t) (Northern, Northern) ((CLeader Relentless), (CLeader NorthCommander))
  where seed t = mkStdGen $ floor $ utctDayTime t
