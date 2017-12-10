-- Part 1

import System.Environment   
import Data.List.Split

convertToInt :: String -> Int
convertToInt x = read x :: Int

parseSpreadsheet :: String -> [[Int]]
parseSpreadsheet string = do
    let rowStrings = filter (/= "") (splitOn "\n" string)
    let columnStrings = map (splitOneOf " \t") rowStrings
    map (map convertToInt) columnStrings

spreadsheetHash :: [[Int]] -> Int
spreadsheetHash spreadsheet = do
    let spreadsheetMaxs = map maximum spreadsheet
    let spreadsheetMins = map minimum spreadsheet
    let spreadsheetDiffs = zipWith (-) spreadsheetMaxs spreadsheetMins
    sum spreadsheetDiffs

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    spreadsheet <- readFile fileName
    print $ spreadsheetHash $ parseSpreadsheet spreadsheet