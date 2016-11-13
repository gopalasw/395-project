module Grammar.PrettyPrint where

import Grammar.Grammar

prettyPrintBoard :: Board -> String
prettyPrintBoard board = "Board: \n" ++ "Player A\n Score:" ++ (show playerAScore) ++ "\n" ++ playerACards ++ "\nPlayer B\n Score:" ++ (show playerBScore) ++ "\n" ++ playerBCards ++ "\n Current Hand: " ++ (show currentHand)
  where
    playerACards = prettyPrintCards (cardsOnBoard (a board))
    playerBCards = prettyPrintCards (cardsOnBoard (b board))
    playerAScore = fst (roundScore board)
    playerBScore = snd (roundScore board)
    currentHand = if (isATurn board) then (cardsInHand (a board)) else (cardsInHand (b board))

prettyPrintCard :: Card -> String
prettyPrintCard (CWeather n row) = "(" ++ n ++ " affects " ++ (show row) ++ ")"
prettyPrintCard (CUnit n row _ _) = "(" ++ n ++ ", " ++ (show row) ++ ")"
prettyPrintCard (CLeader l) = prettyPrintLeader l
prettyPrintCard (CPass) = "Passed"

prettyPrintCards :: [Card] -> String
prettyPrintCards [c] = prettyPrintCard c ++ "\n"
prettyPrintCards (c:cs) = prettyPrintCard c ++ ", " ++ prettyPrintCards cs

prettyPrintRows :: [Row] -> String
prettyPrintRows [r] = (show r)
prettyPrintRows (r:rs) = (show r) ++ ", " ++ (prettyPrintRows rs)

prettyPrintLeader  :: Leader -> String
prettyPrintLeader SteelForged = "SteelForged"
prettyPrintLeader Siegemaster = "Siegemaster"
prettyPrintLeader NorthCommander = "North Commander"
prettyPrintLeader KingTemeria = "King Temeria"
prettyPrintLeader Relentless = "Relentless"
prettyPrintLeader WhiteFlame = "White Flame"
prettyPrintLeader EmperorNilfgaard = "Emperor Nilfgaard"
prettyPrintLeader ImperialMajesty = "Imperial Majesty"

prettyPrintCountry :: Country -> String
prettyPrintCountry Northern = "Northern"
prettyPrintCountry Nilfgaard = "Nilfgaard"
