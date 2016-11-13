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
                zoltan, emiel, villen, avallac] ++ weatherCards

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
