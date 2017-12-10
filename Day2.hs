-- WIP

import System.Environment   
import Control.Arrow
import Data.List  
import Data.List.Split

convertToInt :: String -> Int
convertToInt x = read x :: Int

parseSpreadsheet :: String -> [[Int]]
parseSpreadsheet string = do
    let rowStrings = filter (/= "") (splitOn "\n" string)
    let columnStrings = map (splitOn " ") rowStrings
    map (map convertToInt) columnStrings

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    spreadsheet <- readFile fileName
    print $ parseSpreadsheet spreadsheet