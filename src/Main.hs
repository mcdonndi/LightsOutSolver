--module Main where

import Data.List

type Vector = [Int]
type Row = [Int]
type Matrix = [Row]

testMatrix :: Matrix
testMatrix = [[0,1,0,1,0],[1,0,1,1,1],[0,1,1,1,0],[0,1,0,0,0],[0,0,0,1,1]]

solvable :: Vector -> Bool
solvable initialConfig = if dot1 `mod` 2 == 0 && dot2 `mod` 2 == 0 then True else False
    where   dot1 = dotProduct initialConfig [0,1,1,1,0, 0,1,0,1,0, 1,1,0,1,1, 0,1,0,1,0, 0,1,1,1,0]
            dot2 = dotProduct initialConfig [1,0,1,0,1, 1,0,1,0,1, 0,0,0,0,0, 1,0,1,0,1, 1,0,1,0,1]

dotProduct ::  Vector -> Vector -> Int
dotProduct [] _ = 0
dotProduct _ [] = 0
dotProduct (x:xs) (y:ys) = x*y + dotProduct xs ys

reduceVectorTo23 :: Vector -> Vector
reduceVectorTo23 v = take 23 v

countMoves :: Vector -> Int
countMoves v = sum v

rotateMatrix :: Matrix -> Matrix
rotateMatrix v = fn v
    where fn = reverse . transpose . reverse . transpose

flipVector :: Vector -> Vector
flipVector v = reverse v