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


evalAbility :: Board -> Card -> IO Board
evalAbility board (c@(CUnit _ _ ability _)) = evalAbility' ability
  where
    evalAbility' Scorch = return $ evalScorch board 1
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
    evalAbility' Scorch = return $ evalScorch board 1
    cardsOnPlayersBoard =
      if isATurn board then
        (cardsOnBoard $ a board)
      else
        (cardsOnBoard $ b board)
evalAbility board (CLeader leader) = evalLeader leader
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

peekOpp :: StdGen -> [Card] -> Int -> IO ()
peekOpp seed cards num =
  putStrLn $ show $ map (getName) randomCards
  where
    randomCards =
      case drawCardsR seed num cards of
      (Just drew, left) -> drew
      (Nothing,   left) -> []

playFromDeck :: Card -> Player -> Player
playFromDeck c p =
  p { cardsOnBoard = c : (cardsOnBoard p), cardsLeft = delete c (cardsLeft p) }

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

updateRow :: Card -> Row -> Player -> Player
updateRow (c@(CUnit name _ ability damage)) row p =
  p { cardsOnBoard = (CUnit name row ability damage) : (delete c (cardsOnBoard p)) }
updateRow (c@(CSpecial name _ ability)) row p =
  p { cardsOnBoard = (CSpecial name row ability) : (delete c (cardsOnBoard p)) }

updateDecoy :: Card -> Card -> Player -> Player
updateDecoy (c@(CUnit _ row _ _)) decoy p =
  p { cardsOnBoard = (CSpecial "Decoy" row Decoy) : (delete decoy (delete c (cardsOnBoard p))), cardsInHand = c : delete decoy (cardsInHand p)}

updateUsed :: Card -> Player -> Player
updateUsed c p =
  p { cardsInHand = c : (cardsInHand p), usedCards = delete c (usedCards p) }

evalScorch :: Board -> Row -> Board
evalScorch board r =
  if ((getTotalDamage cardsInRow (weather board)) >= 10) then
    updateOppPlayer board (discardMaxCard r)
  else
    board
  where
    cardsInRow =
      if (isATurn board) then
        (cardsOnBoard (b board))
      else (cardsOnBoard (a board))


evalSpy :: Board -> Card -> Board
evalSpy board spy = updateCurPlayer (updateOppPlayer board (playCard spy)) ((removeCard spy) . (drawFromUsed (randomSeed board) 2))

playCard :: Card -> Player -> Player
playCard card p =
  p { cardsOnBoard = card : (cardsOnBoard p) }

removeCard :: Card -> Player -> Player
removeCard card p =
  p { cardsOnBoard = delete card (cardsOnBoard p) }

removeCardFromUsed :: Card -> Player -> Player
removeCardFromUsed card p =
  p { usedCards = delete card (usedCards p) }

addToHand :: Card -> Player -> Player
addToHand card p =
  p { cardsInHand = card : (cardsInHand p) }

drawFromUsed :: StdGen -> Int -> Player -> Player
drawFromUsed seed n p =
  p { cardsInHand = (fst cards) ++ (cardsInHand p),
      usedCards = snd cards }
  where
    cards =
      case drawCardsR seed n (usedCards p) of
      (Just drew, left) -> (drew, left)
      (Nothing,   left) -> ([],   left)

discardMaxCard :: Row -> Player -> Player
discardMaxCard r p =
  if (cardsInRow == []) then p
  else p { cardsOnBoard = cards', usedCards = maxCard : (usedCards p) }
  where
    cards' = delete maxCard (cardsOnBoard p)
    cardsInRow = filter (cardInRow r) (cardsOnBoard p)
    maxCard = maxDamageCard cardsInRow


maxDamageCard :: [Card] -> Card
maxDamageCard (card:rem) = maxDamage rem card
  where
    maxDamage [] max = max
    maxDamage (c:cs) max =
      if (getDamage Nothing max < getDamage Nothing c) then
        maxDamage cs c
      else
        maxDamage cs max


muster :: Card -> Player -> Player
muster c p = p { cardsOnBoard = (cardsOnBoard p) ++ musterCards,
                    cardsLeft    = filter (c /=) (cardsLeft p),
                    cardsInHand  = filter (c /=) (cardsInHand p) }
  where musterCards = filter (c ==) ((cardsLeft p) ++ (cardsInHand p))


isUnit :: Card -> Bool
isUnit (CUnit _ _ _ _) = True
isUnit _ = False
