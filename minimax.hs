module Minimax where 
import Board

movesLeft :: GameBoard -> Bool
movesLeft [] = False
movesLeft (((_,_),chr):xs)
        | chr == '_' = True
        | otherwise = movesLeft xs

makeNew :: GameBoard -> Int -> Int -> Char -> GameBoard
makeNew currState x y move = foldl (\ newState ((x',y'), chr) -> if x == x' && y == y' then  newState++[((x,y),move)] else newState++[((x',y'),chr)]) [] currState  


minimax :: GameBoard -> Int -> Bool -> Int
minimax gameState depth isMaximizer
       | score == 10 || score == -10 = if isMaximizer then score - depth else score + depth
       | not $ movesLeft gameState = evaluate gameState
       | isMaximizer = foldl (\best ((x,y), c) -> if c == '_' && depth < 2 then max best (minimax (makeNew gameState x y 'x') (depth+1) (not isMaximizer
                              )) else best) (-100) gameState  
       | otherwise = foldl (\best ((x,y), c) -> if c == '_' && depth < 2 then min best (minimax (makeNew gameState x y 'o') (depth+1) (not isMaximizer
                              )) else best) (100) gameState 
       where score = evaluate gameState


alphaBeta :: GameBoard -> Bool -> Int 
alphaBeta gameState isMaximizer 
       | score == 10 || score == -10 = score
       | not $ movesLeft gameState = evaluate gameState
       | isMaximizer = third $ foldl (\(a,b,best) ((x,y), c) -> if c == '_' && b > a
                                                          then
						           let val = (alphaBeta (makeNew gameState x y 'x') False)
							       best' = max best val
							       alpha' = max a best'
							   in
							     (alpha',b,best')
                                                          else (a,b,best)) (-100,100,-100) gameState  
       | otherwise = third $ foldl (\(a,b,best) ((x,y), c) -> if c == '_' && b > a
                                                        then
							    let val = (alphaBeta (makeNew gameState x y 'o') True)
							        best' = min best val
								beta' = min b best'
							    in
							      (a, beta', best')
                                                        else (a,b,best)) (-100,100,100) gameState 
       where score = evaluate gameState
             third (_,_,a) = a



findBestMove :: GameBoard -> (Int,Int,Int)
findBestMove gameState = foldl (\ (bestVal, bestX, bestY) ((x,y), c) -> if c == '_' then let m = minimax (makeNew gameState x y 'x') 0 False 
                                                                          in if m > bestVal then (m,x,y) else (bestVal, bestX,bestY) else (bestVal, bestX, bestY)) (-100,-1,-1) gameState  
