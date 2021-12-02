module Main

import System
import System.File
import Data.String


data Operation = Up Int | Down Int | Forward Int
data Location = Position Int Int

move: Location -> Operation -> Location
move (Position h d) (Up n) = Position (h + n) d
move (Position h d) (Down n) = Position (h - n) d
move (Position h d) (Forward n) = Position h (d + n)

test : List Operation
test = [Forward 5,Down 5,Forward 8,Up 3,Down 8,Forward 2]

runCommands : List Operation -> Location
runCommands = foldl move (Position 0 0)

calculateValue : Location -> Int
calculateValue (Position h d) = h * (- d)


runPt1 : String -> Int


main : IO ()
main = do file <- readFile "resources/2021/day02_test"
          case file of
            Right content => printLn (runPt1 content)
            Left err => printLn err
