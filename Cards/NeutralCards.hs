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
avallac = CUnit "Avallac'h" 1 (Hero Spy) 0

decoy :: Card
decoy = CSpecial "Decoy" 0 Decoy

horn :: Card
horn = CSpecial "Commander's Horn" 0 Horn

torch :: Card
torch = CSpecial "Torch" 1 Scorch
