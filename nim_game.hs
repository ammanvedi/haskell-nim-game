-- Haskell implementation of Nim --
-- Amman Vedi - Haskell Coursework 2 - 2012 --

-- basic operation of the gameplay implementation
-- game state -> player moves -> new game state -> show new game state
-- loops recirsivley until the game state returns a [0,0,0] list

import System.IO
import Data.Char
import Data.List
import Text.Show
import Prelude


initial_board = [3,4,5]


-- main method loads up the game with the default board --
-- also draws the initial game state --
main = do putStrLn " Would you like to play 1 player or 2 player? "
          option <- readLn
          display_game initial_board 3
          if option == 1 then nimai initial_board
                         else nim  initial_board

-- main nim game --
-- runs recursivley until the game is empty "[0,0,0]" then returns
nimai :: [Int] -> IO [Int]
nimai [] = return []
nimai [0,0,0] = return []
nimai xs =  do
             a <- p1 xs
             display_gameai a 1
             b <- ai a
             display_gameai b 2
             nimai b

-- main nim game --
-- runs recursivley until the game is empty "[0,0,0]" then returns --
nim :: [Int] -> IO [Int]
nim [] = return []
nim [0,0,0] = return []
nim xs =  do
             a <- p1 xs
             display_game a 1
             b <- p2 a
             display_game b 2
             nim b


-- take the player 1 input and return the updated state --
-- Current Game State -> update game state -> New Game State --
p1 :: [Int] -> IO [Int]
p1 xs =  do
             putStrLn " "
             putStrLn "Player 1 Enter The Row From Which You Would Like To Take"
             p1r <- readLn
             putStrLn " "
             putStrLn "Player 1 Enter The Amount You Would Like To Take"
             p1t <- readLn
             putStrLn " "
             putStrLn "Nim Game Status: "
             putStrLn " "
             return $ update_game xs p1r p1t


-- take the player 2 input and return the updated state --
-- Current Game State -> update game state -> New Game State --
p2 :: [Int] -> IO [Int]
p2 [0,0,0] = return []
p2 xs =  do
             putStrLn " "
             putStrLn "Player 2 Enter The Row From Which You Would Like To Take"
             p2r <- readLn
             putStrLn " "
             putStrLn "Player 2 Enter The Amount You Would Like To Take"
             p2t <- readLn
             putStrLn " "
             putStrLn "Nim Game Status: "
             putStrLn " "
             return $ update_game xs p2r p2t



-- calculate the computer move and pass it to update_game as if it were a player move --
-- Current Game State -> update game state -> New Game State -
ai :: [Int] -> IO [Int]
ai [0,0,0] = return []
ai xs =  do
             putStrLn " "
             putStrLn "Nim Game Status After Computer Move: "
             putStrLn " "
             air <- ai_row xs
             ait <- ai_take xs air
             return $ update_game xs air ait


-- pick a row to take an item from --
-- Current Game State -> Int --
-- section contains rules for the AI to pick the row from the board --
ai_row :: [Int] -> IO Int
ai_row (x:xs) = do
                   if x > head xs && x > last xs
                     then return $ 1
                     else if head xs > x && head xs > last xs
                         then return $ 2
                         else if last xs > head xs && last xs > x
                             then return $ 3
                             else return $ 1


-- decide how many to take from the board --
-- Current Game State -> the row decided upon -> Int --
-- further rules that contain the actions that should be taken by the AI in --
-- various situations --
ai_take :: [Int] -> Int -> IO Int
ai_take (x:xs) row_picked = do
                           if row_picked == 1 && head xs == 0 && last xs == 0
                             then return $ x
                             else if row_picked == 2 && x == 0 && last xs == 0
                                 then return $ head xs
                                 else if row_picked == 3 && x == 0 && head xs == 0
                                     then return $ last xs
                                     else if row_picked == 1 && head xs == 1 && last xs == 1
                                         then return $ x
                                         else if row_picked == 2 && x == 1 && last xs == 1
                                             then return $ head xs
                                             else if row_picked == 3 && x == 1 && head xs == 1
                                                 then return $ last xs
                                                 else return $ 1


-- display the game state board--
-- Current Game Board -> Which Player -> System Output --
display_game :: [Int] -> Int -> IO ()
display_game [] _ = return()
display_game xs player | sum xs == 0 && player == 1 = putStrLn $ "winner is player 1"
                       | sum xs == 0 && player == 2 = putStrLn $ "winner is player 2"
display_game xs player = do
                    putStrLn $ show (length xs) ++ " -> " ++ replicate (last xs) '|'
                    display_game (init xs) 0

-- display the game state board--
-- Current Game Board -> Which Player -> System Output --
display_gameai :: [Int] -> Int -> IO ()
display_gameai [] _ = return()
display_gameai xs player | sum xs == 0 && player == 1 = putStrLn $ "winner is Player 1"
                       | sum xs == 0 && player == 2 = putStrLn $ "winner is The Computer"
display_gameai xs player = do
                    putStrLn $ show (length xs) ++ " -> " ++ replicate (last xs) '|'
                    display_gameai (init xs) 0

--update the game state --
--Current game state -> user options (row and how many to take) -> new state --
update_game :: [Int] -> Int -> Int -> [Int]
update_game [] _ _ = []
update_game [0,0,0] _ _ = []
update_game (x:xs) row take_amnt | sum(x:xs) == 0 = []
                                 | row == 1 = x - take_amnt:xs
                                 | row == 2 = x : head(xs) - take_amnt : tail(xs)
                                 | row == 3 = x : head(xs) : [last(xs) - take_amnt]





















