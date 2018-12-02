{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (elemIndex)

import Utils

getPuzzle :: IO [Int]
getPuzzle = (map read . words) <$> readFile "day6.txt"

-- | Redistribute the given list.
redistribute :: [Int] -> [Int]
redistribute l = map redistribute' $ zip [0..] l
  where
    len = length l
    high = maximum l
    Just index = elemIndex high l
    base = high `div` len
    extra = index + high `mod` len
    redistribute' (i, v)
      | i == index = base
      | isExtra i = v + base + 1
      | otherwise = v + base
    isExtra i
      | extra < len = i > index && i <= extra
      | otherwise = i > index || i <= extra `mod` len

-- | Continually redistribute the given list to get the first list
-- that cycles and the number of redistribution cycles it takes.
runCycle :: [Int] -> ([Int], Int)
runCycle l = go [] l 0
  where
    go history l acc
      | l `elem` history = (l, acc)
      | otherwise = go (l : history) (redistribute l) $! acc + 1

-- | Given a list, count the number of redistribution cycles before
-- reaching an already-seen list.
countCycles :: [Int] -> Int
countCycles = snd . runCycle

-- | Given a list, count the size of the redistribution loop.
cycleSize :: [Int] -> Int
cycleSize = countCycles . fst . runCycle

main :: IO ()
main = do
  part 1 $ do
    check "redistribute" redistribute
      [ ([0, 2, 7, 0], [2, 4, 1, 2])
      , ([2, 4, 1, 2], [3, 1, 2, 3])
      , ([3, 1, 2, 3], [0, 2, 3, 4])
      , ([0, 2, 3, 4], [1, 3, 4, 1])
      , ([1, 3, 4, 1], [2, 4, 1, 2])
      ]
    check "countCycles" countCycles
      [ ([0, 2, 7, 0], 5)
      ]
    getPuzzle >>= solve . countCycles

  part 2 $ do
    check "cycleSize" cycleSize
      [ ([0, 2, 7, 0], 4)
      ]
    getPuzzle >>= solve . cycleSize
