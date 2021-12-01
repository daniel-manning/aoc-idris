module Main

import System
import System.File
import Data.String


compareDepths : (Int, Int) -> Int -> (Int, Int)
compareDepths (oldDepth, noOfDeeper) depth = if (depth > oldDepth) then (depth, noOfDeeper + 1)
                                      else (depth, noOfDeeper)

noOfIncreasingDepths : List Int -> Int
noOfIncreasingDepths xs = (-1) + snd (foldl compareDepths (0, 0) xs)

slidingWindow3 : List Int -> List (List Int)
slidingWindow3 [] = []
slidingWindow3 (x :: []) = []
slidingWindow3 (x :: y :: []) = []
slidingWindow3 (x :: y :: z :: xs) = [x,y,z] :: slidingWindow3 (y :: z :: xs)

runPt2 : String -> Int
runPt2 s = noOfIncreasingDepths $ map sum $ slidingWindow3 $ map (maybe 0 id . parsePositive) (lines s)

runPt1 : String -> Int
runPt1 s = noOfIncreasingDepths $ map (maybe 0 id . parsePositive) (lines s)

main : IO ()
main = do file <- readFile "resources/2021/day01"
          case file of
               Right content => printLn (runPt2 content)
               Left err => printLn err
