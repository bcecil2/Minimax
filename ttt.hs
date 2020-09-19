import Board
import Minimax

import Data.List
import Data.Char
import Control.Monad.State
import System.IO
showBoard :: GameBoard -> String
showBoard gameState = concat (map (\((_,y), c) -> if y == 3 then "|"++[c]++"|"++"\n" else "|"++[c]++"|") gameState)
board = createBoard "________________"

intToIdx :: Int -> (Int, Int)
intToIdx x = case x of
                1 -> (0,0)
                2 -> (0,1)
                3 -> (0,2)
                4 -> (0,3)
                5 -> (1,0)
                6 -> (1,1)
                7 -> (1,2)
                8 -> (1,3)
                9 -> (2,0)
		10 -> (2,1)
		11 -> (2,2)
		12 -> (2,3)
		13 -> (3,0)
		14 -> (3,1)
		15 -> (3,2)
		16 -> (3,3)
		
updateBoard :: GameBoard -> Int -> Char -> GameBoard
updateBoard gameState idx player = makeNew gameState x y player
                                  where (x,y) = intToIdx idx

getComputersMove :: GameBoard -> (Int,Int)
getComputersMove gameState = let (v,x,y) = findBestMove gameState in (x,y)

isOpen :: (Int,Int) -> GameBoard -> Bool
isOpen (x,y) gameState = elem ((x,y), '_') gameState

gameOver :: GameBoard -> Bool
gameOver gameState = any (==True) $ map ((flip check) gameState) ['x', 'o'] 

isTie :: GameBoard -> Bool
isTie gameState = all (\((_,_),c) -> c /= '_') gameState

playGame :: GameBoard -> IO ()
playGame gameState = do
    if gameOver gameState || isTie gameState
    then putStrLn "Game Over!"
    else do
    putStrLn "Enter a Number 1-16"
    hFlush stdout
    play <- getLine 
    if isOpen (intToIdx $ (read play :: Int)) gameState
       then do
               let newBoard = updateBoard gameState (read play :: Int) 'o'
               putStr $ showBoard newBoard
               putStrLn "----------------"
               hFlush stdout
               let (x,y) = getComputersMove newBoard; nB = makeNew newBoard x y 'x'
               putStr $ showBoard nB
               putStrLn "----------------"
               hFlush stdout
               playGame nB
       else do
              putStrLn "Theres already a piece there!"
	      hFlush stdout
	      playGame gameState
main = do 
    playGame board	
