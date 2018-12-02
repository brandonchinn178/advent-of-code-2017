module Main where

import Utils

import Data.List (subsequences)

type Row = [Int]
type Spreadsheet = [Row]

getPuzzle :: IO Spreadsheet
getPuzzle = (map (map read . words) . lines) <$> readFile "day2.txt"

getCheckSum :: (Row -> Int) -> Spreadsheet -> Int
getCheckSum getRowCheckSum = sum . map getRowCheckSum

part1Check :: Row -> Int
part1Check row = maximum row - minimum row

part2Check :: Row -> Int
part2Check = go . allPairs
  where
    go ((a, b):rest) =
      let
        high = max a b
        low = min a b
      in if high `mod` low == 0
        then high `div` low
        else go rest
    go _ = error "invalid row"

main :: IO ()
main = do
  part 1 $ do
    let input = [[5,1,9,5],[7,5,3],[2,4,6,8]]
    check "getCheckSum" (getCheckSum part1Check) [(input, 18)]
    getPuzzle >>= solve . getCheckSum part1Check

  part 2 $ do
    let input = [[5,9,2,8],[9,4,7,3],[3,8,6,5]]
    check "getCheckSum" (getCheckSum part2Check) [(input, 9)]
    getPuzzle >>= solve . getCheckSum part2Check
