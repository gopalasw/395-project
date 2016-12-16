module Cards.Cards where

import Grammar.Grammar
import Cards.NeutralCards
import Cards.NorthernCards
import Cards.NilfgaardCards

-- helper function to find row card is in
cardInRow :: Row -> Card -> Bool
cardInRow r (CUnit _ row _ _) = r == row
cardInRow r (CSpecial _ row _) = r == row
cardInRow r (CWeather _ row) = r == row
cardInRow r _ = False

-- all neutral cards
neutralCards :: [Card]
neutralCards = [geralt, cirilla, vesemir,
                yennefer, triss, dandelion,
                zoltan, emiel, villen, avallac,
                decoy, decoy, decoy,
                torch, torch, torch,
                horn, horn, horn] ++ weatherCards

-- all weather cards
weatherCards :: [Card]
weatherCards = concat $ map (take 3 . repeat) [frost, fog, rain, clear]

-- northern deck
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

-- nilfgaard deck
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