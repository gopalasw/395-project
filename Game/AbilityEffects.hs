module Game.AbilityEffects where

import Grammar.Grammar
import Grammar.Board
import Grammar.PrettyPrint
import Cards.Cards
import Game.Basics
import Game.UserInput
import Data.List
import System.Random
import Grammar.Board


-- updates board with additional damage caused by abilities
cardsAbilityDamage :: Board -> Board
cardsAbilityDamage board = board { roundScore = ((fst $ roundScore board) + aDamage, (snd $ roundScore board) + bDamage) }
  where
    aCards = (cardsOnBoard (a board))
    bCards = (cardsOnBoard (b board))
    aDamage = sum $ map (abilityDamage board aCards) aCards
    bDamage = sum $ map (abilityDamage board bCards) bCards

-- calculates additional damage caused by a specific card
abilityDamage :: Board -> [Card] -> Card -> Int
abilityDamage board cards card = ability card
  where
    ability (CUnit _ row Morale _) = length (filter (\c -> (cardInRow row c) && (isUnit c)) cards) - 1
    ability (c@(CUnit _ row Bond damage)) = damage * (length (filter (c ==) cards) - 1)
    ability (CUnit _ r Horn _) = getTotalDamage (filter (cardInRow r) cards) (weather board)
    ability (CUnit name row (Hero a) damage) = ability (CUnit name row a damage)
    ability (CLeader Siegemaster) = getTotalDamage (filter (cardInRow 3) cards) (weather board)
    ability (CSpecial _ r Horn) = getTotalDamage (filter (cardInRow r) cards) (weather board)
    ability _ = 0


-- evaluates immediate effect of playing a card in that round
evalAbility :: Board -> Card -> IO Board
evalAbility board (c@(CUnit _ _ ability _)) = evalAbility' ability
  where
    evalAbility' Scorch = return $ evalScorch board 0
    evalAbility' Spy = return $ evalSpy board c
    evalAbility' (Hero ability) = evalAbility' ability
    evalAbility' Medic =
      if cardsPlayerUsed == [] then
        putStrLn "No cards in used pile. " >> return board
      else do
        card <- getCardHelper cardsPlayerUsed getDrawIndex
        return $ updateCurPlayer board $ updateUsed card
    evalAbility' Agile = do
      r <- getRow
      return $ updateCurPlayer board (updateRow c r)
    evalAbility' Muster = return $ updateCurPlayer board (muster c)
    evalAbility' _ = return board
    cardsPlayerUsed =
      if isATurn board then
        (usedCards $ a board)
      else
        (usedCards $ b board)
evalAbility board (c@(CSpecial _ _ ability)) = evalAbility' ability
  where
    evalAbility' Decoy =
      if cardsOnPlayersBoard == [] then
        putStrLn "No cards on board. " >> return board
      else do
        card <- getCardHelper cardsOnPlayersBoard getSwapIndex
        return $ updateCurPlayer board $ updateDecoy card c
    evalAbility' Horn = do
      r <- getRow
      return $ updateCurPlayer board (updateRow c r)
    evalAbility' Scorch = return $ updateCurPlayer (evalScorch board 1) (removeCard c)
    cardsOnPlayersBoard =
      if isATurn board then
        (cardsOnBoard $ a board)
      else
        (cardsOnBoard $ b board)
evalAbility board (CLeader leader) = usedLeaderBoard $ evalLeader leader
  where
    evalLeader SteelForged = return $ evalScorch board 3
    evalLeader NorthCommander =
      return $ updateWeather board clear
    evalLeader KingTemeria =
      return $ playWeather board fog
    evalLeader Relentless =
      if cardsOppUsed == [] then
        putStrLn "No cards in used pile. " >> return board
      else do
        card <- getCardHelper cardsOppUsed getDrawIndex
        return $ updateOppPlayer (updateCurPlayer board $ addToHand card)
                               (removeCardFromUsed card)
    evalLeader EmperorNilfgaard = do
      peekOpp (randomSeed board) oppHand 3
      return board
    evalLeader ImperialMajesty =
      return $ playWeather board rain
    evalLeader _ = return board
    cardsOppUsed =
      if isATurn board then
        (usedCards $ b board)
      else
        (usedCards $ a board)
    oppHand =
      if isATurn board then
        (cardsInHand $ b board)
      else
        (cardsInHand $ a board)
evalAbility board _ = return board

-- if the leader has been used, update board to reflect this
usedLeaderBoard :: IO Board -> IO Board
usedLeaderBoard ioboard = do
  board <- ioboard
  return $ updateCurPlayer board updateUsedLeader

-- update flag to show leader been used
updateUsedLeader :: Player -> Player
updateUsedLeader p =
  p { leader = ((fst $ leader p), True) }

-- peek num random cards from list 
peekOpp :: StdGen -> [Card] -> Int -> IO ()
peekOpp seed cards num =
  putStrLn $ show $ map (getName) randomCards
  where
    randomCards =
      case drawCardsR seed num cards of
      (Just drew, left) -> drew
      (Nothing,   left) -> []

-- take a card from the deck and add to board
playFromDeck :: Card -> Player -> Player
playFromDeck c p =
  p { cardsOnBoard = c : (cardsOnBoard p), cardsLeft = delete c (cardsLeft p) }

-- update board with new weather
playWeather :: Board -> Card -> Board
playWeather board card =
  if elem fog deck then
    updateCurPlayer (updateWeather board card) (playFromDeck card)
  else
    board
  where
    deck =
      if isATurn board then
        (cardsLeft $ a board)
      else
        (cardsLeft $ b board)

-- updates the row of a card. used for cards that have user-determined rows
updateRow :: Card -> Row -> Player -> Player
updateRow (c@(CUnit name _ ability damage)) row p =
  p { cardsOnBoard = (CUnit name row ability damage) : (delete c (cardsOnBoard p)) }
updateRow (c@(CSpecial name _ ability)) row p =
  p { cardsOnBoard = (CSpecial name row ability) : (delete c (cardsOnBoard p)) }

-- if decoy is played properly, adds to board and readds card to hand.
  -- otherwise, deletes decoy from board and places in hand
updateDecoy :: Card -> Card -> Player -> Player
updateDecoy (c@(CUnit _ row _ _)) decoy p =
  p { cardsOnBoard = (CSpecial "Decoy" row Decoy) : (delete decoy (delete c (cardsOnBoard p))), cardsInHand = c : delete decoy (cardsInHand p)}
updateDecoy card decoy p =
  p { cardsOnBoard = (delete decoy (cardsOnBoard p)) }

-- move a card from used to hand
updateUsed :: Card -> Player -> Player
updateUsed c p =
  p { cardsInHand = c : (cardsInHand p), usedCards = delete c (usedCards p) }

-- evaluate scorch: if row is 0, searches through all rows.
  -- otherwise, only applies on specified row
evalScorch :: Board -> Row -> Board
evalScorch board 0 =
  updateCurPlayer (updateOppPlayer board
                                  (discardMaxCards (weather board) maxCard))
                  (discardMaxCards (weather board) maxCard)

  where
    allCards = (cardsOnBoard $ a board) ++ (cardsOnBoard $ b board)
    maxCard = (maxDamageCard (weather board) allCards)
evalScorch board r =
  if ((getTotalDamage cardsInRow (weather board)) >= 10) then
    updateOppPlayer board (discardMaxCard (weather board) r)
  else
    board
  where
    cardsInRow =
      if (isATurn board) then
        (cardsOnBoard (b board))
      else (cardsOnBoard (a board))

-- adds spy to opp players board and draws a card
evalSpy :: Board -> Card -> Board
evalSpy board spy = updateCurPlayer (updateOppPlayer board (playCard spy)) ((removeCard spy) . (drawFromUsed (randomSeed board) 2))

-- adds card to board
playCard :: Card -> Player -> Player
playCard card p =
  p { cardsOnBoard = card : (cardsOnBoard p) }

-- removes card from board
removeCard :: Card -> Player -> Player
removeCard card p =
  p { cardsOnBoard = delete card (cardsOnBoard p) }

-- removes card from used pile
removeCardFromUsed :: Card -> Player -> Player
removeCardFromUsed card p =
  p { usedCards = delete card (usedCards p) }

-- adds card to hand
addToHand :: Card -> Player -> Player
addToHand card p =
  p { cardsInHand = card : (cardsInHand p) }

-- uses random generator to draw n random cards from used pile
drawFromUsed :: StdGen -> Int -> Player -> Player
drawFromUsed seed n p =
  p { cardsInHand = (fst cards) ++ (cardsInHand p),
      usedCards = snd cards }
  where
    cards =
      case drawCardsR seed n (usedCards p) of
      (Just drew, left) -> (drew, left)
      (Nothing,   left) -> ([],   left)

-- discards the max card in a row
discardMaxCard :: Weather -> Row -> Player -> Player
discardMaxCard w 0 p =
  if (allCards == []) then p
  else p { cardsOnBoard = cards', usedCards = maxCard : (usedCards p) }
  where
    cards' = delete maxCard (cardsOnBoard p)
    allCards = cardsOnBoard p
    maxCard = maxDamageCard w allCards
discardMaxCard w r p =
  if (cardsInRow == []) then p
  else p { cardsOnBoard = cards', usedCards = maxCard : (usedCards p) }
  where
    cards' = delete maxCard (cardsOnBoard p)
    cardsInRow = filter (cardInRow r) (cardsOnBoard p)
    maxCard = maxDamageCard w cardsInRow

-- discards max cards from board (if same damage, can be multiple)
discardMaxCards :: Weather -> Card -> Player -> Player
discardMaxCards weather max p =
  if (allCards == []) then p
  else p { cardsOnBoard = cards', usedCards = maxCards ++ (usedCards p) }
  where
    cards' = filter (\x -> not (x `elem` maxCards)) (cardsOnBoard p)
    allCards = cardsOnBoard p
    maxCards = filter (sameDamage max) allCards
    sameDamage c1 c2 = (getDamage (Just weather) c1) == (getDamage (Just weather) c2)

-- finds the card with max damage in a list
maxDamageCard :: Weather -> [Card] -> Card
maxDamageCard weather (card:rem) = maxDamage rem card
  where
    maxDamage [] max = max
    maxDamage (c:cs) max =
      if (getDamage (Just weather) max < getDamage (Just weather) c) then
        maxDamage cs c
      else
        maxDamage cs max

-- draws all cards of same type from deck
muster :: Card -> Player -> Player
muster c p = p { cardsOnBoard = (cardsOnBoard p) ++ musterCards,
                    cardsLeft    = filter (c /=) (cardsLeft p),
                    cardsInHand  = filter (c /=) (cardsInHand p) }
  where musterCards = filter (c ==) ((cardsLeft p) ++ (cardsInHand p))


-- finds if card is a unit
isUnit :: Card -> Bool
isUnit (CUnit _ _ _ _) = True
isUnit _ = False
