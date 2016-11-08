module Cards.NeutralCards where

import Grammar.Grammar

geralt :: Card
geralt = CUnit "Geralt of Rivia" 1 (Hero None) 10

cirilla :: Card
cirilla = CUnit "Cirilla Fiona Elen Rianno" 1 (Hero None) 15

vesemir :: Card
vesemir = CUnit "Vesemir" 1 None 6

yennefer :: Card
yennefer = CUnit "Yennefer of Vengenberg" 2 (Hero Medic) 7

triss :: Card
triss = CUnit "Triss Merigold Gwent" 1 (Hero None) 7

dandelion :: Card
dandelion = CUnit "Dandelion" 1 Morale 2

zoltan :: Card
zoltan = CUnit "Zoltan Chivay" 1 None 5

emiel :: Card
emiel = CUnit "Emiel Regis Rohellec Terzieff" 1 None 5

villen :: Card
villen = CUnit "Villentretenmerth" 1 Scorch 7

avallac :: Card
avallac = CUnit "Avallac’h" 1 (Hero Spy) 0

--decoy :: Card
--decoy = CUnit "Decoy" 

-- there are 3 of each weather card
frost :: Card
frost = CWeather "Biting Frost" 1

fog :: Card
fog = CWeather "Impenetrable Fog" 2

rain :: Card
rain = CWeather "Torrential Rain" 3

clear :: Card
clear = CWeather "Clear Weather" 0


{-
Decoy Gwent Card  Decoy —-  —-  —-  20  Bought from Trader  Velen Gwent Quest: Collect ’em all! Quartermaster’s, Baron’s Store, Crow’s Perch
Decoy Gwent Card  Decoy —-  —-  —-  20  Bought from Trader  Novigrad (Gustfields) Gwent Quest: Collect ’em all! From a Trader at Seven Cats Inn
Decoy Gwent Card  Decoy —-  —-  —-  50  Bought from Trader  White Orchard Gwent Quest: Collect ’em all! From an Innkeeperess at White Orchard Tavern
Commander’s Horn Gwent Card Commander’s Horn  —-  —-  —-  10  Bought from Trader  Velen Gwent Quest: Collect ’em all! From an Innkeeper at Inn at the Crossroads
Commander’s Horn Gwent Card Commander’s Horn  —-  —-  —-  10  Bought from Trader  Novigrad  Gwent Quest: Collect ’em all! From a Store Trader at Passiflora
Commander’s Horn Gwent Card Commander’s Horn  —-  —-  —-  20  Bought from Trader  Novigrad (Oxenfurt) Gwent Quest: Collect ’em all! From Stjepan at The Alchemy Inn, Oxenfurt
Torch Gwent Card  Scorch  —-  —-  —-  50  Bought from Trader  Novigrad (Grassy Knoll) Gwent Quest: Collect ’em all! From an Innkeeper, Cunny of the Goose
Torch Gwent Card  Scorch  —-  —-  —-  50  Bought from Trader  Skellige  Gwent Quest: Collect ’em all! From an Innkeeper at New Port Inn, Kaer Trolde Harbor
Torch Gwent Card  Scorch  —-  —-  —-  50  Bought from Trader  Skellige  Gwent Quest: Collect ’em all! From an Innkeeper at Urialla Village, An Skellig
-}