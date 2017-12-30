-- Part 1

import System.Environment   
import Data.List.Split

quot' :: Integer -> [Integer] -> Integer
quot' x (y:rest)
    | x `rem` y == 0    = x `quot` y
    | y `rem` x == 0    = y `quot` x
    | null rest         = 0
    | otherwise         = quot' x rest

-- Takes 
quotientOfDivisiblePair :: [Integer] -> Integer
quotientOfDivisiblePair (x:rest)
    | quotX /=0 = quotX
    | otherwise = quotientOfDivisiblePair rest
    where quotX = quot' x rest

spreadsheetHash :: [[Integer]] -> Integer
spreadsheetHash spreadsheet = sum $ map quotientOfDivisiblePair spreadsheet

convertToInteger :: String -> Integer
convertToInteger x = read x :: Integer

parseSpreadsheet :: String -> [[Integer]]
parseSpreadsheet string = do
    let rowStrings = filter (/= "") (splitOn "\n" string)
    let columnStrings = map (splitOneOf " \t") rowStrings
    map (map convertToInteger) columnStrings

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    spreadsheet <- readFile fileName
    print $ spreadsheetHash $ parseSpreadsheet spreadsheet