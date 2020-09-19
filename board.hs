module Board where

import qualified Data.Map as Map

type GameBoard = [((Int,Int),Char)]

createBoard :: String -> GameBoard 
createBoard board = zip idxs board
           where idxs = [(x,y) | x <- [0..3], y <- [0..3]]

fetchRow :: Int -> GameBoard -> String
fetchRow rowNum gameState = foldl(\ row ((x,y),chr) -> if x == rowNum then row ++ [chr] else row) "" gameState   

fetchCol :: Int -> GameBoard -> String
fetchCol colNum gameState = foldl(\ col ((x,y),chr) -> if y == colNum then col ++ [chr] else col) "" gameState 

fetchDiag :: GameBoard -> String
fetchDiag gameState = foldl(\ diag ((x,y),chr) -> if x == y then diag ++ [chr] else diag) "" gameState

fetchMinorDiag :: GameBoard -> String
fetchMinorDiag gameState = foldl (\ diag ((x,y),chr) -> if elem (x,y) [(0,3),(1,2),(2,1),(3,0)] then diag ++ [chr] else diag) "" gameState

count :: Char -> String -> Int
count needle haystack = foldl (\ sum c -> if c == needle then sum + 1 else sum) 0 haystack

fetchRows :: GameBoard -> [String]
fetchRows gameState = map (\x -> fetchRow x gameState) [0..3]

fetchCols :: GameBoard -> [String]
fetchCols gameState = map (\x -> fetchCol x gameState) [0..3]

check :: Char -> GameBoard -> Bool
check sym gameState = any (\x -> count sym x == 4) $ concat [fetchRows gameState, fetchCols gameState, [fetchDiag gameState], [fetchMinorDiag gameState]]

evaluate :: GameBoard -> Int
evaluate gameState
        | x = 10
        | o = -10
        | not(x && o) = 0
        | x && o = 0
        where x = check 'x' gameState
              o = check 'o' gameState

gameState = createBoard "xxo_oxo_o"

