module Cards.Cards where

import Grammar.Grammar
import Cards.NeutralCards
import Cards.NorthernCards
import Cards.NilfgaardCards

cardInRow :: Row -> Card -> Bool
cardInRow r (CUnit _ row _ _) = r == row
cardInRow r (CWeather _ row) = r == row
cardInRow r _ = False

neutralCards :: [Card]
neutralCards = [geralt, cirilla, vesemir,
                yennefer, triss, dandelion,
                zoltan, emiel, villen, avallac,
                decoy, decoy, decoy,
                torch, torch, torch,
                horn, horn, horn] ++ weatherCards

weatherCards :: [Card]
weatherCards = concat $ map (take 3 . repeat) [frost, fog, rain, clear]

northernCards :: [Card]
northernCards = [vernon, john, esterad, philippa,
                 thaler, ves, siegfried, yarpen,
                 sigismund, keira, sile, sabrina,
                 sheldon, dethmold, stennis,
                 footSoldier, dun, tower,
                 trebuchet, trebuchet,
                 infantry, infantry, infantry,
                 crinfrid, crinfrid, crinfrid,
                 catapult, catapult, ballista, ballista,
                 siegeExpert, siegeExpert, siegeExpert,
                 blueStripes, blueStripes, blueStripes] ++ neutralCards

nilfgaardCards :: [Card]
nilfgaardCards = [letho, menno, morvran, tibor,
                  albrich, assire, cynthia,
                  fringilla, morteisen, rainfarn,
                  renuald, rotten, shilard, stefan,
                  sweers, vanhemar, vattier, vreemde,
                  cahir, puttkammer, auxArcher, auxArcher,
                  infantryArcher, infantryArcher,
                  seigeTech, siegeEngineer,
                  fireScorpion, fireScorpion,
                  emissary, emissary,
                  impera, impera, impera, impera,
                  nausicaa, nausicaa, nausicaa] ++ neutralCards

-- there are 3 of each weather card
frost :: Card
frost = CWeather "Biting Frost" 1

fog :: Card
fog = CWeather "Impenetrable Fog" 2

rain :: Card
rain = CWeather "Torrential Rain" 3

clear :: Card
clear = CWeather "Clear Weather" 0

hashFunc :: Card -> Int
hashFunc (CUnit name _ _ _) =
  case name of
    "Geralt of Rivia"                 -> 0
    "Cirilla Fiona Elen Rianno"       -> 1
    "Vesemir"                         -> 2
    "Yennefer of Vengenberg"          -> 3
    "Triss Merigold Gwent"            -> 4
    "Dandelion"                       -> 5
    "Zoltan Chivay"                   -> 6 
    "Emiel Regis Rohellec Terzieff"   -> 7
    "Villentretenmerth"               -> 8
    "Avallac’h"                       -> 9
    "Letho of Gulet"                  -> 10
    "Menno Coehoorn"                  -> 11
    "Morvran Voorhis"                 -> 12
    "Tibor Eggebracht"                -> 13
    "Albrich"                         -> 14
    "Assire var Anahid"               -> 15
    "Cynthia"                         -> 16
    "Fringilla Vigo"                  -> 17
    "Morteisen"                       -> 18
    "Rainfarn"                        -> 19
    "Renuald aep Matsen"              -> 20
    "Rotten Mangonel"                 -> 21
    "Shilard Fitz-Oesterlen"          -> 22
    "Stefan Skellen"                  -> 23
    "Sweers"                          -> 24
    "Vanhemar"                        -> 25
    "Vattier de Rideaux"              -> 26
    "Vreemde"                         -> 27
    "Cahir Mawr Dyffryn aep Ceallach" -> 28
    "Puttkammer"                      -> 29
    "Etolian Auxiliary Archers"       -> 30
    "Black Infantry Archer"           -> 31
    "Siege Technican"                 -> 32
    "Zerrikanian Fire Scorpion"       -> 33
    "Impera Brigade"                  -> 34
    "Nausicaa Cavalry"                -> 35
    "Siege Engineer"                  -> 36
    "Young Emissary"                  -> 37
    "Vernon Roche"                    -> 38
    "John Natalis"                    -> 39
    "Esterad Thyssen"                 -> 40
    "Philippa Eilhart"                -> 41
    "Thaler"                          -> 42
    "Ves"                             -> 43
    "Siegfried of Denesle"            -> 44
    "Yarpen Zigrin"                   -> 45
    "Sigismund Dijkstra"              -> 46
    "Keira Metz"                      -> 47
    "Sile de Tansarville"             -> 48
    "Sabrina Glevissig"               -> 49
    "Sheldon Skaggs"                  -> 50
    "Dethmold"                        -> 51
    "Trebuchet"                       -> 52
    "Poor F*cking Infantry"           -> 53
    "Prince Stennis"                  -> 54
    "Crinfrid Reavers Dragon Hunter"  -> 55
    "Redanian Foot Soldier"           -> 56
    "Catapult"                        -> 57
    "Ballista"                        -> 58
    "Kaedweni Siege Expert"           -> 59
    "Blue Stripes"                    -> 60
    "Siege Tower"                     -> 61
    "Dun Banner Medic"                -> 62
hashFunc (CWeather name _) =
  case name of
    "Biting Frost"                    -> 63
    "Impenetrable Fog"                -> 64
    "Torrential Rain"                 -> 65
    "Clear Weather"                   -> 66
hashFunc (CSpecial ability) =
  case ability of
    Decoy                             -> 67 
    Horn                              -> 68
    Scorch                            -> 69
hashFunc CPass =                         70
hashFunc (CLeader ability) =
  case ability of
    SteelForged                       -> 0
    Siegemaster                       -> 1
    NorthCommander                    -> 2
    KingTemeria                       -> 3
    Relentless                        -> 4
    WhiteFlame                        -> 5
    EmperorNilfgaard                  -> 6
    ImperialMajesty                   -> 7
    Canceled                          -> 8
