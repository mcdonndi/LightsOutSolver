--module Main where

import Data.List

type Vector = [Int]
type Row = [Int]
type Matrix = [Row]

sampleInput :: Vector
sampleInput = [0,1,0,1,0, 1,0,1,1,1, 0,1,1,1,0, 0,1,0,0,0, 0,0,0,1,1]

testMatrix :: Matrix
testMatrix = [[0,1,0,1,0],[1,0,1,1,1],[0,1,1,1,0],[0,1,0,0,0],[0,0,0,1,1]]

quietPattern1 :: Vector
quietPattern1 = [0,1,1,1,0, 1,0,1,0,1, 1,1,0,1,1, 1,0,1,0,1, 0,1,1,1,0]

quietPattern2 :: Vector
quietPattern2 = [1,0,1,0,1, 1,0,1,0,1, 0,0,0,0,0, 1,0,1,0,1, 1,0,1,0,1]

quietPattern3 :: Vector
quietPattern3 = [1,1,0,1,1, 0,0,0,0,0, 1,1,0,1,1, 0,0,0,0,0, 1,1,0,1,1]

solvable :: Vector -> Bool
solvable initialConfig = if dot1 `mod` 2 == 0 && dot2 `mod` 2 == 0 then True else False
    where   dot1 = dotProduct initialConfig quietPattern1
            dot2 = dotProduct initialConfig quietPattern2

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

applyQuietPattern :: Vector -> Vector -> Vector
applyQuietPattern v qp = zipWith xor v qp

xor :: Int -> Int -> Int
xor 0 0 = 0
xor 0 1 = 1
xor 1 0 = 1
xor 1 1 = 0