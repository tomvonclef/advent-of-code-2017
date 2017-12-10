module ArrowFun where
import Control.Arrow

input :: Integer
input = 123123

breakIntoSingleDigits :: Integer -> [Integer]
breakIntoSingleDigits integer =
  if integer == 0 then
    []
  else
    breakIntoSingleDigits (integer `quot` 10)
      ++ [integer `mod` 10]

matchAhead = quot (length (breakIntoSingleDigits input)) 2

prependLast :: Int -> [Integer] -> [Integer]
prependLast numToPrepend list =
  reverse (take numToPrepend (reverse list)) ++ list

circularMatches :: [Integer] -> [Integer]
circularMatches list = do
  let prependedList = prependLast matchAhead list
  [if first == second then first else 0
    | i <- [0 .. (length prependedList - (matchAhead + 1))]
    , let first = prependedList !! i
    , let second = prependedList !! (i + matchAhead)]

circularSum :: Integer -> Integer
circularSum = breakIntoSingleDigits >>> circularMatches >>> sum

main = do
  print (circularSum input)
