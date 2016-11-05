module Game.Cards where

import Grammar.Grammar
import Grammar.PrettyPrint


allCards :: [Card]
allCards =
  [geralt, cirilla, villen, vernon, trebuchet, infantry, albrich, fringilla, menno, frost, clear]

geralt   :: Card
geralt   = CUnit "Geralt of Rivia" 1 Hero 10

cirilla  :: Card
cirilla  = CUnit "Cirilla Fiona Elen Rianno" 1 Hero 15

villen   :: Card
villen   = CUnit "Villentretenmerth" 1 (Scorch 1) 7

vernon   :: Card
vernon   = CUnit "Vernon Roche" 1 Hero 10

trebuchet :: Card
trebuchet = CUnit "Trebuchet" 3 None 6

infantry :: Card
infantry = CUnit "Poor F*cking Infantry" 1 Bond 1

albrich  :: Card
albrich  = CUnit "Albrich" 2 None 2

fringilla :: Card
fringilla = CUnit "Fringilla Vigo" 2 None 6

menno    :: Card
menno    = CUnit "Menno Coehoorn" 1 Hero 10

frost :: Card
frost = CWeather "Biting Frost" [1]

clear :: Card
clear = CWeather "Clear Weather" [1, 2, 3]