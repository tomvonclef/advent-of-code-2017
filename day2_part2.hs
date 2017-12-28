-- Part 1

import System.Environment   
import Data.List
import Data.List.Split

-- Lazy list of primes from https://stackoverflow.com/a/3596536
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `rem` p > 0]

--isFactorOf :: Integer -> Integer -> Bool
--isFactorOf mightBeFactor number = number `rem` mightBeFactor == 0

firstFactorIn :: Integer -> [Integer] -> Integer
firstFactorIn x xs = do
    let maybeFound = find (\potentialFactor -> x `rem` potentialFactor == 0) xs
    maybe 0 id maybeFound

primeFactors :: Integer -> [Integer]
primeFactors x
    | x == 0 || x == 1 = []
    | otherwise = firstFactor : primeFactors (x `quot` firstFactor)
    where firstFactor = firstFactorIn x primes

uniquePrimeFactors :: Integer -> [Integer]
uniquePrimeFactors x = nub (primeFactors x)

convertToInteger :: String -> Integer
convertToInteger x = read x :: Integer

parseSpreadsheet :: String -> [[Integer]]
parseSpreadsheet string = do
    let rowStrings = filter (/= "") (splitOn "\n" string)
    let columnStrings = map (splitOneOf " \t") rowStrings
    map (map convertToInteger) columnStrings

spreadsheetHash :: [[Integer]] -> Integer
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