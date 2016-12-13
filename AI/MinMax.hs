module MinMax where
import Data.List
import Grammar.Grammar
import Grammar.Board

data Tree a = Node a [Tree a] deriving (Show)

------------------------------------------------------------------------------
-- And uses this as his data structure for his game tree implementation in
-- section 5.  Read through section 5 and implement everything necessary to
-- implement the initial working `evaluate` function in the paper.  You may copy
-- code verbatim from the paper, although you will find that not all of it works
-- as advertised!  This is because Hughes wrote this paper with the predecesor
-- to Haskell, Miranda, in mind.  In general when dealing with any academic
-- work, we'll need to put in some effort to translate what they did into our
-- particular context.
--
-- As a final note, for any function that you add to your program, please give
-- it an appropriate type signature.  Hughes did not include types in his code
-- which helps with the presentation but hinders good Haskell style.
--
-- Demonstrate that your code works by creating five boards by hand and costing
-- them using the `evaluate` function.
--------------------------------------------------------------------------------

-- Insert part 2 functions here
--

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
-- Part 3: Alpha-Beta Pruning
--
-- Finally, work through the rest of section 5 to add alpha-beta pruning to your
-- implementation.  You should implement the modified `evaluate` function that
-- uses alpha-beta pruning.  In addition to alpha-beta pruning, implement the
-- sort-and-take-top-n heuristic that follows.  You do not need to implement the
-- dynamic heuristic that ends the section.
-- Demonstrate that your code works by costing the five boards you created in
-- part 2 with your updated evaluation function.  You can use a version of
-- evaluate that uses both the alpha-beta pruning and sort-and-take-top-n
-- optimizations.
--------------------------------------------------------------------------------
-- Insert part 3 functions here

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
evaluateAB board = (evalfuncAB board) . highfirst . maptree static . prune 8 . gametree $ board
