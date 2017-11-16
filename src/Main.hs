--module Main where

solvable :: [Int] -> Bool
solvable initialConfig = if dot1 `mod` 2 == 0 && dot2 `mod` 2 == 0 then True else False
    where   dot1 = dotProduct initialConfig [0,1,1,1,0, 0,1,0,1,0, 1,1,0,1,1, 0,1,0,1,0, 0,1,1,1,0]
            dot2 = dotProduct initialConfig [1,0,1,0,1, 1,0,1,0,1, 0,0,0,0,0, 1,0,1,0,1, 1,0,1,0,1]

dotProduct ::  [Int] -> [Int] -> Int
dotProduct [] _ = 0
dotProduct _ [] = 0
dotProduct (x:xs) (y:ys) = x*y + dotProduct xs ys