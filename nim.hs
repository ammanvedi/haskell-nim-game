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
main = do putStrLn " "
          display_game initial_board 3
          nim initial_board


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


-- display the game state board--
-- Current Game Board -> Which Player -> System Output --
display_game :: [Int] -> Int -> IO ()
display_game [] _ = return()
display_game xs player | sum xs == 0 && player == 1 = putStrLn $ "winner is player 1"
                       | sum xs == 0 && player == 2 = putStrLn $ "winner is player 2"
display_game xs player = do
                    putStrLn $ show (length xs) ++ " -> " ++ replicate (last xs) '|'
                    display_game (init xs) 0


--update the game state --
--Current game state -> user options (row and how many to take) -> new state --
update_game :: [Int] -> Int -> Int -> [Int]
update_game [] _ _ = []
update_game [0,0,0] _ _ = []
update_game (x:xs) row take_amnt | sum(x:xs) == 0 = []
                                 | row == 1 = x - take_amnt:xs
                                 | row == 2 = x : head(xs) - take_amnt : tail(xs)
                                 | row == 3 = x : head(xs) : [last(xs) - take_amnt]

