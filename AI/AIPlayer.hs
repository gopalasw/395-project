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

thisPlayerToIA :: Player -> [Int]
thisPlayerToIA p = handArr ++ leftArr ++ onBoardArr ++ usedArr ++ livesArr
                   ++ leaderP ++ countryP
  where
    initCardsToIA :: [Card] -> [Int]
    initCardsToIA cs = cardsToIA initialIA cs
    handArr = initCardsToIA $ cardsInHand p
    leftArr = initCardsToIA $ cardsLeft p
    onBoardArr = initCardsToIA $ cardsOnBoard p
    usedArr = initCardsToIA $ usedCards p
    livesArr = lives p
    leaderP = (hashFun (leader p)) : []
    countryP = case country p of
                Nilfgaard -> [0] 
                Northern  -> [1]

otherPlayerToIA :: Player -> [Int]
otherPlayerToIA p = onBoardArr ++ usedArr
  where
    initCardsToIA :: [Card] -> [Int]
    initCardsToIA cs = cardsToIA initialIA cs
    onBoardArr = initCardsToIA $ cardsOnBoard p
    usedArr = initCardsToIA $ usedCards p
    leaderP = (hashFun (leader p)) : []
    countryP = case country p of
                Nilfgaard -> [0]
                Northern  -> [1]



