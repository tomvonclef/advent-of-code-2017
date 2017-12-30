-- Part 2

import System.Environment   
import Data.List.Split

evenlyDivisibleQuotient :: Integer -> [Integer] -> Integer
evenlyDivisibleQuotient x (y:rest)
    | x `rem` y == 0    = x `quot` y
    | y `rem` x == 0    = y `quot` x
    | null rest         = 0
    | otherwise         = evenlyDivisibleQuotient x rest

quotientOfDivisiblePair :: [Integer] -> Integer
quotientOfDivisiblePair (x:rest) = do
    let quotX = evenlyDivisibleQuotient x rest
    if quotX /=0 then quotX else quotientOfDivisiblePair rest

spreadsheetHash :: [[Integer]] -> Integer
spreadsheetHash spreadsheet = sum $ map quotientOfDivisiblePair spreadsheet

parseSpreadsheet :: String -> [[Integer]]
parseSpreadsheet string = do
    let rowStrings = filter (/= "") (splitOn "\n" string)
    let columnStrings = map (splitOneOf " \t") rowStrings
    map (map read) columnStrings

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    spreadsheet <- readFile fileName
    print $ spreadsheetHash $ parseSpreadsheet spreadsheet