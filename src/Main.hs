--module Main where

import Data.List

type Vector = [Int]
type Row = [Int]
type Matrix = [Row]

solvableInput :: Vector
solvableInput = [1,0,0,0,0, 1,0,1,0,1, 1,0,0,0,1, 1,0,1,0,1, 0,0,0,0,1]

testMatrix :: Matrix
testMatrix = [[0,1,0,1,0],[1,0,1,1,1],[0,1,1,1,0],[0,1,0,0,0],[0,0,0,1,1]]

quietPattern1 :: Vector
quietPattern1 = [0,1,1,1,0, 1,0,1,0,1, 1,1,0,1,1, 1,0,1,0,1, 0,1,1,1,0]

quietPattern2 :: Vector
quietPattern2 = [1,0,1,0,1, 1,0,1,0,1, 0,0,0,0,0, 1,0,1,0,1, 1,0,1,0,1]

quietPattern3 :: Vector
quietPattern3 = [1,1,0,1,1, 0,0,0,0,0, 1,1,0,1,1, 0,0,0,0,0, 1,1,0,1,1]

row4 :: Vector
row4 = [1,1,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,1,1,1]

reducedAInverse :: Matrix
reducedAInverse =   [[0,1,1,1,0,0,0,1,0,1,0,0,0,1,1,0,0,0,0,1,0,0,0],
                    [1,1,0,1,1,0,1,0,0,0,0,0,1,1,1,0,0,0,1,0,0,0,0],
                    [1,0,1,1,1,1,0,1,1,0,0,0,1,1,0,1,1,1,1,1,0,1,0],
                    [1,1,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,1,1,1],
                    [0,1,1,0,1,1,0,0,0,0,1,0,1,0,1,0,0,1,0,1,1,1,0],
                    [0,0,1,0,1,0,1,1,0,1,0,0,1,0,0,0,0,0,1,1,0,0,0],
                    [0,1,0,1,0,1,1,0,1,1,0,0,0,1,0,1,1,1,0,0,0,1,0],
                    [1,0,1,0,0,1,0,1,1,0,0,0,0,0,1,1,0,1,0,1,1,0,1],
                    [0,0,1,0,0,0,1,1,1,0,1,0,0,1,1,1,0,0,1,0,0,1,1],
                    [1,0,0,0,0,1,1,0,0,0,1,0,1,0,1,0,1,1,0,1,0,0,1],
                    [0,0,0,0,1,0,0,0,1,1,0,0,1,0,1,1,1,1,1,0,0,1,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,1,1],
                    [0,1,1,0,1,1,0,0,0,1,1,0,1,1,0,0,0,1,0,0,1,1,0],
                    [1,1,1,0,0,0,1,0,1,0,0,0,1,1,1,0,0,0,1,0,0,0,0],
                    [1,1,0,0,1,0,0,1,1,1,1,0,0,1,1,1,1,0,1,0,1,0,0],
                    [0,0,1,0,0,0,1,1,1,0,1,0,0,0,1,1,0,1,0,1,1,0,1],
                    [0,0,1,1,0,0,1,0,0,1,1,1,0,0,1,0,1,1,1,0,0,0,1],
                    [0,0,1,0,1,0,1,1,0,1,1,0,1,0,0,1,1,0,1,1,1,0,0],
                    [0,1,1,0,0,1,0,0,1,0,1,0,0,1,1,0,1,1,1,0,0,0,1],
                    [1,0,1,0,1,1,0,1,0,1,0,0,0,0,0,1,0,1,0,1,1,0,1],
                    [0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,1,0,1,1,1,0],
                    [0,0,1,1,1,0,1,0,1,0,1,1,1,0,0,0,0,0,0,0,1,1,1],
                    [0,0,0,1,0,0,0,1,1,1,0,1,0,0,0,1,1,0,1,1,0,1,0]]

solve :: Vector -> Maybe Vector
solve v = if solvable v == False
    then Nothing
    else Just (multiplyByVector reducedAInverse (reduceVectorTo23 v))

solvable :: Vector -> Bool
solvable initialConfig = if dot1 == 0 && dot2 == 0 then True else False
    where   dot1 = dotProduct initialConfig quietPattern1
            dot2 = dotProduct initialConfig quietPattern2

dotProduct ::  Vector -> Vector -> Int
dotProduct [] _ = 0
dotProduct _ [] = 0
dotProduct (x:xs) (y:ys) = (x*y) `xor` (dotProduct xs ys)

reduceVectorTo23 :: Vector -> Vector
reduceVectorTo23 v = take 23 v

countMoves :: Vector -> Int
countMoves v = sum v

rotateMatrix :: Matrix -> Matrix
rotateMatrix v = fn v
    where fn = reverse . transpose . reverse . transpose

flipVector :: Vector -> Vector
flipVector v = reverse v

multiplyByVector :: Matrix -> Vector -> Vector
multiplyByVector [] _ = [0]
multiplyByVector _ [] = [0]
multiplyByVector (m:ms) v = (dotProduct m v) : (multiplyByVector ms v)

-- Potential infinite loop
rowStartWithOne :: Matrix -> Matrix
rowStartWithOne (m:ms) = if x == 1 then (m:ms) else rowStartWithOne ms ++ [m]
    where x = head m

applyQuietPattern :: Vector -> Vector -> Vector
applyQuietPattern v qp = zipWith xor v qp

xor :: Int -> Int -> Int
xor 0 0 = 0
xor 0 1 = 1
xor 1 0 = 1
xor 1 1 = 0