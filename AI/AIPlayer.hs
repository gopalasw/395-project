module AI.AIPlayer where

import Grammar.Grammar
import Grammar.Board

hashFun :: Card -> Int
hashFun = undefined

initialIA :: [Int]
initialIA = take 71 $ repeat 0

cardsToIA :: [Int] -> [Card] -> [Int]
cardsToIA ls []     = ls
cardsToIA ls (c:cs) = cardsToIA updateLs cs
  where 
    index    = hashFun c
    updateLs = addByIndex ls index


addByIndex :: [Int] -> Int -> [Int]
addByIndex ls index = take index  ls ++ ( 
                      (head (drop index ls) + 1) :
                      drop (index + 1) ls)

weatherToIA :: Weather -> [Int]
weatherToIA (b1, b2, b3) = [bToI b1, bToI b2, bToI b3]
  where
    bToI :: Bool -> Int
    bToI True  = 1
    bToI False = 0

roundScoreToIA :: (Int, Int) -> [Int]
roundScoreToIA (i1, i2) =  [i1, i2]

thisPlayerToIA :: Player -> [Int]
thisPlayerToIA p = handArr ++ leftArr ++ onBoardArr ++ usedArr ++ livesArr
                   ++ leader ++ country
  where
    initCardsToIA :: [Card] -> [Int]
    initCardsToIA cs = cardsToIA initialIA cs
    handArr = initCardsToIA $ cardsInHand p
    leftArr = initCardsToIA $ cardsLeft p
    onBoardArr = initCardsToIA $ cardsOnBoard p
    usedArr = initCardsToIA $ usedCards p
    livesArr = lives p
    leader :: [Int]
    leader = (hashFun (leader p)) : []
    country = case country p of
                Nilfgaard -> [0] 
                Northern  -> [1]


