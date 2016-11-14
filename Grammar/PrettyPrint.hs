module Grammar.PrettyPrint where

import Grammar.Grammar
import Grammar.Board
import Cards.Cards

prettyPrintBoard :: Board -> String
prettyPrintBoard board =
  "Board: \n" ++ "Player A\n Score: " ++
  (show playerAScore) ++ "\n" ++ playerACards ++
  "\nPlayer B\n Score: " ++ (show playerBScore) ++
  "\n" ++ playerBCards ++ "\n Current Hand:\n" ++ (prettyPrintCards currentHand)
  where
    playerACards = prettyPrintCards (cardsOnBoard (a board))
    playerBCards = prettyPrintCards (cardsOnBoard (b board))
    playerAScore = fst (roundScore board)
    playerBScore = snd (roundScore board)
    currentHand  = if (isATurn board) then (cardsInHand (a board)) else (cardsInHand (b board))


prettyPrintStatus :: Board -> String
prettyPrintStatus board =
     "\nPlayer A's Lives: " ++ (show (lives $ a board))
  ++ " Player B's Lives: " ++ (show (lives $ b board))
  ++ "\n"


prettyPrintCard :: Card -> String
prettyPrintCard (CWeather n row)   = n ++ " "
prettyPrintCard (CUnit n _  _ dmg) = n ++ " (D: " ++ (show dmg) ++ ") "
prettyPrintCard (CLeader l)        = prettyPrintLeader l
prettyPrintCard (CPass)            = "Passed"

prettyPrintCards :: [Card] -> String
prettyPrintCards [] = ""
prettyPrintCards cards = concat $ zipWith prettyPrintRow [0..3] rows
  where
    zeroes = filter (cardInRow 0) cards
    ones   = filter (cardInRow 1) cards
    twos   = filter (cardInRow 2) cards
    threes = filter (cardInRow 3) cards
    rows   = [zeroes, ones, twos, threes]

prettyPrintRow :: Row -> [Card] -> String
prettyPrintRow r c = show r ++ ": " ++ (concat (map prettyPrintCard c)) ++ "\n"

prettyPrintLeader  :: Leader -> String
prettyPrintLeader SteelForged      = "SteelForged"
prettyPrintLeader Siegemaster      = "Siegemaster"
prettyPrintLeader NorthCommander   = "North Commander"
prettyPrintLeader KingTemeria      = "King Temeria"
prettyPrintLeader Relentless       = "Relentless"
prettyPrintLeader WhiteFlame       = "White Flame"
prettyPrintLeader EmperorNilfgaard = "Emperor Nilfgaard"
prettyPrintLeader ImperialMajesty  = "Imperial Majesty"
prettyPrintLeader Canceled         = "Your leader's ability has been canceled."

prettyPrintCountry :: Country -> String
prettyPrintCountry Northern  = "Northern"
prettyPrintCountry Nilfgaard = "Nilfgaard"
