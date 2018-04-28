--module Main where

import Data.List
import Data.Function

type Vector = [Int]
type Row = [Int]
type Matrix = [Row]

solvableInput :: Vector
solvableInput = [1,0,0,0,0, 1,0,1,0,1, 1,0,0,0,1, 1,0,1,0,1, 0,0,0,0,1]

testMatrix :: Matrix
testMatrix = [[0,1,0,1,0],[1,0,1,1,1],[0,1,1,1,0],[0,1,0,0,0],[0,0,0,1,1]]

lightsOutMatrix :: Matrix
lightsOutMatrix =  [[1,1,0,0,0, 1,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0],
                    [1,1,1,0,0, 0,1,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0],
                    [0,1,1,1,0, 0,0,1,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0],
                    [0,0,1,1,1, 0,0,0,1,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0],
                    [0,0,0,1,1, 0,0,0,0,1, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0],

                    [1,0,0,0,0, 1,1,0,0,0, 1,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0],
                    [0,1,0,0,0, 1,1,1,0,0, 0,1,0,0,0, 0,0,0,0,0, 0,0,0,0,0],
                    [0,0,1,0,0, 0,1,1,1,0, 0,0,1,0,0, 0,0,0,0,0, 0,0,0,0,0],
                    [0,0,0,1,0, 0,0,1,1,1, 0,0,0,1,0, 0,0,0,0,0, 0,0,0,0,0],
                    [0,0,0,0,1, 0,0,0,1,1, 0,0,0,0,1, 0,0,0,0,0, 0,0,0,0,0],

                    [0,0,0,0,0, 1,0,0,0,0, 1,1,0,0,0, 1,0,0,0,0, 0,0,0,0,0],
                    [0,0,0,0,0, 0,1,0,0,0, 1,1,1,0,0, 0,1,0,0,0, 0,0,0,0,0],
                    [0,0,0,0,0, 0,0,1,0,0, 0,1,1,1,0, 0,0,1,0,0, 0,0,0,0,0],
                    [0,0,0,0,0, 0,0,0,1,0, 0,0,1,1,1, 0,0,0,1,0, 0,0,0,0,0],
                    [0,0,0,0,0, 0,0,0,0,1, 0,0,0,1,1, 0,0,0,0,1, 0,0,0,0,0],

                    [0,0,0,0,0, 0,0,0,0,0, 1,0,0,0,0, 1,1,0,0,0, 1,0,0,0,0],
                    [0,0,0,0,0, 0,0,0,0,0, 0,1,0,0,0, 1,1,1,0,0, 0,1,0,0,0],
                    [0,0,0,0,0, 0,0,0,0,0, 0,0,1,0,0, 0,1,1,1,0, 0,0,1,0,0],
                    [0,0,0,0,0, 0,0,0,0,0, 0,0,0,1,0, 0,0,1,1,1, 0,0,0,1,0],
                    [0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,1, 0,0,0,1,1, 0,0,0,0,1],

                    [0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 1,0,0,0,0, 1,1,0,0,0],
                    [0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,1,0,0,0, 1,1,1,0,0],
                    [0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,1,0,0, 0,1,1,1,0],
                    [0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,1,0, 0,0,1,1,1],
                    [0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,1, 0,0,0,1,1]]

identityMatrix25 :: Matrix
identityMatrix25 =  [[1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]]

vectorToMatrix :: Vector -> Matrix
vectorToMatrix [] = []
vectorToMatrix v = [take n v] ++ vectorToMatrix (drop n v)
    where   n = floor . sqrt . fromIntegral $ l
            l = length v

matrixToVector :: Matrix -> Vector
matrixToVector [] = []
matrixToVector (m:ms) = m ++ matrixToVector ms

solve :: Vector -> Maybe Vector
solve v = if solvable v == False
    then Nothing
    else Just (getLeastMovesSolution $ solutionsAndMoveCount $ createSolutionsList $ appendTwoZeroes $ multiplyByVector (reduceMatrixTo23 aInverse) (reduceVectorTo23 v))
    where (pseudoIdentity, aInverse) = gaussJordan (lightsOutMatrix, identityMatrix25) 25

solvable :: Vector -> Bool
solvable initialConfig = if dot1 == 0 && dot2 == 0 then True else False
    where   dot1 = dotProduct initialConfig qp1
            dot2 = dotProduct initialConfig qp2
            (qp1, qp2) = getQuietPatterns

getQuietPatterns :: (Vector, Vector)
getQuietPatterns = (last (init aInverse), last aInverse)
    where (pseudoIdentity, aInverse) = gaussJordan (lightsOutMatrix, identityMatrix25) 25

dotProduct ::  Vector -> Vector -> Int
dotProduct [] _ = 0
dotProduct _ [] = 0
dotProduct (x:xs) (y:ys) = (x*y) `xor` (dotProduct xs ys)

createSolutionsList :: Vector -> [Vector]
createSolutionsList v = [v, (v `xorVectors` qp1), (v `xorVectors` qp2), (v `xorVectors` qp3)]
    where   (qp1, qp2) = getQuietPatterns
            qp3 = qp1 `xorVectors` qp2

solutionsAndMoveCount :: [Vector] -> [(Vector, Int)]
solutionsAndMoveCount [] = []
solutionsAndMoveCount (v:vs) = (v, countMoves v) : solutionsAndMoveCount vs

getLeastMovesSolution :: [(Vector, Int)] -> Vector
getLeastMovesSolution list =  fst $ head $ sortBy (compare `on` snd) list

reduceVectorTo23 :: Vector -> Vector
reduceVectorTo23 v = take 23 v

reduceMatrixTo23 :: Matrix -> Matrix
reduceMatrixTo23 m = map reduceVectorTo23 newM
    where newM = take 23 m

appendTwoZeroes :: Vector -> Vector
appendTwoZeroes v = v ++ [0,0]

countMoves :: Vector -> Int
countMoves v = sum v

rotateMatrix :: Matrix -> Matrix
rotateMatrix v = fn v
    where fn = reverse . transpose . reverse . transpose

flipVector :: Vector -> Vector
flipVector v = reverse v

multiplyByVector :: Matrix -> Vector -> Vector
multiplyByVector [] _ = []
multiplyByVector _ [] = []
multiplyByVector (m:ms) v = (dotProduct m v) : (multiplyByVector ms v)

gaussJordan :: (Matrix, Matrix) -> Int -> (Matrix, Matrix)
gaussJordan ([], _) _ = ([], [])
gaussJordan (_, []) _ = ([], [])
gaussJordan (m1, m2) n = (m1Step3, m2Step5)
    where   m2Step5 = xorMatrixRowsWithVectorsWithSecondLastOne (last (init m2Step4)) (init (init m2Step4)) ++ [last (init m2Step4)] ++ [last m2Step4]
            m2Step4 = xorMatrixRowsWithVectorsWithLastOne (last m2Step3) (init m2Step3) ++ [last m2Step3]
            (m1Step3, m2Step3) = reducedRowEchelon (m1Step2, m2Step1)
            m1Step2 = addZeroesToMatrix m1Step1 n
            (m1Step1, m2Step1) = rowEchelon (m1, m2)

rowEchelon :: (Matrix, Matrix) -> (Matrix, Matrix)
rowEchelon ([], _) = ([], [])
rowEchelon (_, []) = ([], [])
rowEchelon (m1, m2) = ([head newM1] ++ newM1Tail, [head newM2] ++ newM2Tail)
    where   (newM1Tail, newM2Tail) = rowEchelon (removeFirstColumn $ tail newM1, tail newM2)
            (newM1, newM2) = xorWithFirstRow (rowStartWithOne (m1, m2))

reducedRowEchelon :: (Matrix, Matrix) -> (Matrix, Matrix)
reducedRowEchelon ([], _) = ([], [])
reducedRowEchelon (_, []) = ([], [])
reducedRowEchelon (m1, m2) = if numAllZeroRowsEqualsNumColumns m1 == True || matrixIsEmpty m1 then (m1, m2) else (addColumnToLeftOfMatrix m1Column1 m1Part3, m2Part3)
    where   (m1Part3, m2Part3) = reducedRowEchelon (m1LessColumn1, m2Rejoin)
            m1Column1 = getFirstColumn m1Part2
            m1LessColumn1 = removeFirstColumn m1Rejoin
            m1Rejoin = m1Part2 ++ m1Part1Bottom
            m2Rejoin = m2Part2 ++ m2Part1Bottom
            (m1Part2, m2Part2) = xorWithLastRow (m1Part1Top, m2Part1Top)
            (m1Part1Top, m2Part1Top) = getRowUpToLastLeadingOne (m1, m2)
            (m1Part1Bottom, m2Part1Bottom) = getRowAfterLastLeadingOne (m1, m2)

getRowAfterLastLeadingOne :: (Matrix, Matrix) -> (Matrix, Matrix)
getRowAfterLastLeadingOne ([], _) = ([], [])
getRowAfterLastLeadingOne (_, []) = ([], [])
getRowAfterLastLeadingOne (m1, m2) = if head (last m1) == 1 then ([], []) else (initM1 ++ [last m1], initM2 ++ [last m2])
    where (initM1, initM2) = getRowAfterLastLeadingOne (init m1, init m2)

getRowUpToLastLeadingOne :: (Matrix, Matrix) -> (Matrix, Matrix)
getRowUpToLastLeadingOne ([], _) = ([], [])
getRowUpToLastLeadingOne (_, []) = ([], [])
getRowUpToLastLeadingOne (m1, m2) = if head (last m1) == 1 then (m1, m2) else getRowUpToLastLeadingOne (init m1, init m2)

getFirstColumn :: Matrix -> Matrix
getFirstColumn [] = []
getFirstColumn (m:ms) = [[head m]] ++ getFirstColumn ms

addColumnToLeftOfMatrix :: Matrix -> Matrix -> Matrix
addColumnToLeftOfMatrix col [] = col
addColumnToLeftOfMatrix [] (m:ms) = [[0] ++ m] ++ addColumnToLeftOfMatrix [] ms
addColumnToLeftOfMatrix (col:cols) (m:ms) = [col ++ m] ++ addColumnToLeftOfMatrix cols ms

addZeroesToMatrix :: Matrix -> Int -> Matrix
addZeroesToMatrix [] _ = []
addZeroesToMatrix (m:ms) n = [addZeroesToVector m (n - (length m))] ++ addZeroesToMatrix ms n

addZeroesToVector :: Vector -> Int -> Vector
addZeroesToVector v n = replicate n 0 ++ v

checkForAllLeadingZeroes :: Matrix -> Bool
checkForAllLeadingZeroes [] = True
checkForAllLeadingZeroes (m:ms) = hasLeadingZero && checkForAllLeadingZeroes ms
    where hasLeadingZero = if head m == 0 then True else False

rowStartWithOne :: (Matrix, Matrix) -> (Matrix, Matrix)
rowStartWithOne ([], _) = ([], [])
rowStartWithOne (_, []) = ([], [])
rowStartWithOne (m:ms, n:ns) = if head m == 1 then (m:ms, n:ns) else if checkForAllLeadingZeroes (m:ms) == True then (m:ms, n:ns) else rowStartWithOne (ms ++ [m], ns ++ [n])

xorWithFirstRow :: (Matrix, Matrix) -> (Matrix, Matrix)
xorWithFirstRow ([], _) = ([], [])
xorWithFirstRow (_, []) = ([], [])
xorWithFirstRow (m:ms, n:ns) = ([m] ++ newMs, [n] ++ newNs)
    where (newMs, newNs) = xorMatrixRowsWithVectorsWithHeadOne (m, n) (ms, ns)

xorWithLastRow :: (Matrix, Matrix) -> (Matrix, Matrix)
xorWithLastRow ([], _) = ([], [])
xorWithLastRow (_, []) = ([], [])
xorWithLastRow (m1, m2) = (newM1 ++ [last m1], newM2 ++ [last m2])
    where (newM1, newM2) = xorMatrixRowsWithVectorsWithHeadOne (last m1, last m2) (init m1, init m2)

removeFirstColumn :: Matrix-> Matrix
removeFirstColumn [] = []
removeFirstColumn m = map tail m

xorMatrixRowsWithVectorsWithHeadOne :: (Vector, Vector) -> (Matrix, Matrix) -> (Matrix, Matrix)
xorMatrixRowsWithVectorsWithHeadOne _ (_, []) = ([], [])
xorMatrixRowsWithVectorsWithHeadOne _ ([], _) = ([], [])
xorMatrixRowsWithVectorsWithHeadOne (v1, v2) (m1, m2) = ([m1Head] ++ m1Tail, [m2Head] ++ m2Tail)
    where   (m1Head, m2Head) = xorWithVectorsWithHeadOne (v1, v2) (head m1, head m2)
            (m1Tail, m2Tail) = xorMatrixRowsWithVectorsWithHeadOne (v1, v2) (tail m1, tail m2)

xorWithVectorsWithHeadOne :: (Vector, Vector)-> (Vector, Vector) -> (Vector, Vector)
xorWithVectorsWithHeadOne (v1, v2) (v3, v4) = if head v3 == 1 then (xorVectors v1 v3, xorVectors v2 v4) else (v3, v4)

xorMatrixRowsWithVectorsWithLastOne :: Vector -> Matrix -> Matrix
xorMatrixRowsWithVectorsWithLastOne _ [] = []
xorMatrixRowsWithVectorsWithLastOne v m =  mInit ++ [mLast]
    where   mLast = xorWithVectorWithLastOne v (last m)
            mInit = xorMatrixRowsWithVectorsWithLastOne v (init m)

xorWithVectorWithLastOne :: Vector -> Vector-> Vector
xorWithVectorWithLastOne v1 v2 = if last v2 == 1 then xorVectors v1 v2 else v2

xorMatrixRowsWithVectorsWithSecondLastOne :: Vector -> Matrix -> Matrix
xorMatrixRowsWithVectorsWithSecondLastOne _ [] = []
xorMatrixRowsWithVectorsWithSecondLastOne v m =  mInit ++ [mLast]
    where   mLast = xorWithVectorWithSecondLastOne v (last m)
            mInit = xorMatrixRowsWithVectorsWithSecondLastOne v (init m)

xorWithVectorWithSecondLastOne :: Vector -> Vector-> Vector
xorWithVectorWithSecondLastOne v1 v2 = if last (init v2) == 1 then xorVectors v1 v2 else v2

xorVectors :: Vector -> Vector -> Vector
xorVectors v1 v2 = zipWith xor v1 v2

xor :: Int -> Int -> Int
xor 0 0 = 0
xor 0 1 = 1
xor 1 0 = 1
xor 1 1 = 0

vectorIsEmpty :: Vector -> Bool
vectorIsEmpty v = if v == [] then True else False

matrixIsEmpty :: Matrix -> Bool
matrixIsEmpty [] = True
matrixIsEmpty (m:ms) = vectorIsEmpty m && matrixIsEmpty ms

numAllZeroRowsEqualsNumColumns :: Matrix -> Bool
numAllZeroRowsEqualsNumColumns m = if countZeroVectorsAtBottom m == length (head m) then True else False

countZeroVectorsAtBottom :: Matrix -> Int
countZeroVectorsAtBottom [] = 0
countZeroVectorsAtBottom m = if sum (last m) == 0 then 1 + (countZeroVectorsAtBottom $ init m) else 0