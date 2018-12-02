{-# LANGUAGE LambdaCase #-}

module Main where

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM

import Utils

getPuzzle :: IO [Int]
getPuzzle = (map read . lines) <$> readFile "day5.txt"

type Values = HashMap Int Int
type State = (Values, Int)

-- | The initial state.
initState :: [Int] -> State
initState list = (HM.fromList $ zip [0..] list, 0)

-- | Get the next state given the current state.
nextState :: (Int -> Int) -> State -> Maybe State
nextState incFunc (values, index) = nextState' <$> HM.lookup index values
  where
    nextState' v = (HM.insert index (incFunc v) values, index + v)

totalSteps :: (Int -> Int) -> [Int] -> Int
totalSteps incFunc list = count $ iterate (>>= nextState incFunc) initState'
  where
    initState' = Just $ initState list
    count (_:l) = count' l 0 --  first state is initial state
    count' (next:rest) acc = case next of
      Nothing -> acc
      Just _ -> count' rest $! acc + 1

part1IncFunc :: Int -> Int
part1IncFunc = (1 +)

part2IncFunc :: Int -> Int
part2IncFunc x = if x >= 3 then x - 1 else x + 1

main :: IO ()
main = do
  part 1 $ do
    check "totalSteps1" totalSteps1
      [ ([0, 3, 0, 1, -3], 5)
      ]
    getPuzzle >>= solve . totalSteps1

  part 2 $ do
    check "totalSteps2" totalSteps2
      [ ([0, 3, 0, 1, -3], 10)
      ]
    getPuzzle >>= solve . totalSteps2
  where
    totalSteps1 = totalSteps part1IncFunc
    totalSteps2 = totalSteps part2IncFunc
