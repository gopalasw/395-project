module Game.Round where

import Control.Applicative
import Grammar.Board
import Grammar.Grammar
import Grammar.PrettyPrint
import Game.Basics
import Game.Turn
import Cards.Cards

-- sequence of actions for each round
roundSeq :: IO Board -> IO Board
roundSeq board = do
  b <- board
  b <- turnLoop $ roundStart b True
  b <- pure $ evaluateRound b
  putStrLn $ prettyPrintStatus b
  return b

-- starts the round with an empty board
roundStart :: Board -> Bool -> IO Board
roundStart b@(Board p1 p2 _ _ _ _) bool = do
  b <- pure
     $ b { a = p1 { usedCards = (filterPass $ cardsOnBoard p1) ++ (usedCards p1),
                    cardsOnBoard = [], cardsInHand = (filterPass $ cardsInHand p1) ++ [CPass] },
           b = p2 { usedCards = (filterPass $ cardsOnBoard p2) ++ (usedCards p2),
               cardsOnBoard = [], cardsInHand = (filterPass $ cardsInHand p2) ++ [CPass] },
           weather = (False, False, False),
           roundScore   = (0, 0),
           isATurn   = bool}
  putStrLn $ prettyPrintBoard b
  return b
  where
    filterPass :: [Card] -> [Card]
    filterPass [] = []
    filterPass (CPass:cs) = filterPass cs
    filterPass (c:cs) = c : (filterPass cs)

-- evaluates round to see who won round
evaluateRound :: Board -> Board
evaluateRound b@(Board p1 p2 _ (s1, s2) _ _)
  | s1 < s2  = b { a = p1 { lives = 0 : (lives p1)},
                   b = p2 { lives = 1 : (lives p2)}}
  | s1 > s2  = b { a = p1 { lives = 1 : (lives p1)},
                   b = p2 { lives = 0 : (lives p2)}}
  | s1 == s2 = b { a = p1 { lives = 0 : (lives p1)},
                   b = p2 { lives = 0 : (lives p2)}}


