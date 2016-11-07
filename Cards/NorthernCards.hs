module Cards.NorthernCards where

import Grammar.Grammar

vernon :: Card
vernon = CUnit "Vernon Roche" 1 (Hero None) 10

john :: Card
john = CUnit "John Natalis" 1 (Hero None) 10

esterad :: Card
esterad = CUnit "Esterad Thyssen" 1 (Hero None) 10

philippa :: Card
philippa = CUnit "Philippa Eilhart" 2 (Hero None) 10

thaler :: Card
thaler = CUnit "Thaler" 3 Spy 1

ves :: Card
ves = CUnit "Ves" 1 None 5

siegfried :: Card
siegfried = CUnit "Siegfried of Denesle" 1 None 5

yarpen :: Card
yarpen = CUnit "Yarpen Zigrin" 1 None 2

sigismund :: Card
sigismund = CUnit "Sigismund Dijkstra" 1 Spy 4

keira :: Card
keira = CUnit "Keira Metz" 2 None 5

sile :: Card
sile = CUnit "Sile de Tansarville" 2 None 5

sabrina :: Card
sabrina = CUnit "Sabrina Glevissig" 2 None 4

sheldon :: Card
sheldon = CUnit "Sheldon Skaggs" 2 None 4

dethmold :: Card
dethmold = CUnit "Dethmold" 2 None 6

-- total: 2
trebuchet :: Card
trebuchet = CUnit "Trebuchet" 3 None 6

-- total: 3
infantry :: Card
infantry = CUnit "Poor F*cking Infantry" 1 Bond 1

stennis :: Card
stennis = CUnit "Prince Stennis" 1 Spy 1 

-- total: 3
crinfrid :: Card
crinfrid = CUnit "Crinfrid Reavers Dragon Hunter" 2 Bond 5

footSoldier :: Card
footSoldier = CUnit "Redanian Foot Soldier" 1 None 1

-- total: 2
catapult :: Card
catapult = CUnit "Catapult" 3 Bond 8

-- total: 2
ballista :: Card
ballista = CUnit "Ballista" 3 None 6

-- total: 3
siegeExpert :: Card
siegeExpert = CUnit "Kaedweni Siege Expert" 3 Morale 1

-- total: 3
blueStripes :: Card
blueStripes = CUnit "Blue Stripes" 1 Bond 4

tower :: Card
tower = CUnit "Siege Tower" 3 None 6

dun :: Card
dun = CUnit "Dun Banner Medic" 3 Medic 5