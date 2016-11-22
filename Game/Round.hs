module Game.Round where

import Grammar.Board
import Grammar.Grammar
import Grammar.PrettyPrint
import Game.Basics
import Game.Turn
import Cards.Cards

roundSeq :: IO Board -> IO Board
roundSeq board = do
  b <- board
  b <- turnLoop $ pure $ roundStart b True -- TODO: Change this Bool to accurately represent who starts rounds
  b <- pure $ evaluateRound b
  putStrLn $ prettyPrintStatus b
  return b

roundStart :: Board -> Bool -> Board
roundStart b@(Board p1 p2 _ _ _ _) bool =
  b { a = p1 { usedCards = (cardsOnBoard p1) ++ (usedCards p1),
               cardsOnBoard = []},
      b = p2 { usedCards = (cardsOnBoard p2) ++ (usedCards p2),
               cardsOnBoard = []},
      weather = (False, False, False),
      roundScore   = (0, 0),
      isATurn   = bool}


evaluateRound :: Board -> Board
evaluateRound b@(Board p1 p2 _ (s1, s2) _ _)
  | s1 < s2  = b { a = p1 { lives = 0 : (lives p1)},
                   b = p2 { lives = 1 : (lives p2)}}
  | s1 > s2  = b { a = p1 { lives = 1 : (lives p1)},
                   b = p2 { lives = 0 : (lives p2)}}
  | s1 == s2 = b { a = p1 { lives = 0 : (lives p1)},
                   b = p2 { lives = 0 : (lives p2)}}


