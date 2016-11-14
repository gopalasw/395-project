module Game.Round where

import Grammar.Board
import Grammar.Grammar
import Game.Basics
import Game.Turn
import Cards.Cards

roundLoop :: IO Board -> IO Board
roundLoop board = do
  b <- board
  b <- turnLoop $ pure $ roundStart b True -- TODO: Change this Bool to accurately represent who starts rounds
  return $ evaluateRound b

roundStart :: Board -> Bool -> Board
roundStart b@(Board p1 p2 _ _ _ _) bool =
  b { a = p1 { usedCards = (cardsOnBoard p1) ++ (usedCards p1),
               cardsOnBoard = []},
      b = p2 { usedCards = (cardsOnBoard p2) ++ (usedCards p2),
               cardsOnBoard = []},
      weather = [],
      roundScore   = (0, 0),
      isATurn   = bool}


evaluateRound :: Board -> Board
evaluateRound b@(Board p1 p2 _ (s1, s2) _ _)
  | s1 < s2  = b { a = p1 { score = 0 : (score p1)},
                   b = p2 { score = 1 : (score p2)}}
  | s1 > s2  = b { a = p1 { score = 1 : (score p1)},
                   b = p2 { score = 0 : (score p2)}}
  | s1 == s2 = b { a = p1 { score = 0 : (score p1)},
                   b = p2 { score = 0 : (score p2)}}


