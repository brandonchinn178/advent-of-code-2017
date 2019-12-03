{- stack script
    --resolver lts-14.12
-}

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  input <- readFile "Day3.txt"
  let (cmds1, cmds2) = splitCommands input
      pointsMap1 = toPointsMap cmds1
      pointsMap2 = toPointsMap cmds2

  print $ part1 pointsMap1 pointsMap2
  print $ part2 pointsMap1 pointsMap2

part1 :: PointsMap -> PointsMap -> Int
part1 pointsMap1 pointsMap2 = minimum $ Map.intersectionWithKey manhattanDist pointsMap1 pointsMap2
  where
    manhattanDist (x, y) _ _ = abs x + abs y

part2 :: PointsMap -> PointsMap -> Int
part2 pointsMap1 pointsMap2 = minimum $ Map.intersectionWithKey addSteps pointsMap1 pointsMap2
  where
    addSteps _ steps1 steps2 = steps1 + steps2

-- ((dx, dy), number of steps)
type Command = ((Int, Int), Int)

-- A list of visited points, mapped to the number of steps it took to get there.
-- If a point was visited multiple times, keep the lowest number of steps
type PointsMap = Map (Int, Int) Int

splitCommands :: String -> ([Command], [Command])
splitCommands = toPair . map (map toCommand . splitOn ",") . lines
  where
    toPair = \case
      [x, y] -> (x, y)
      l -> error $ "Not a pair: " ++ show l

toCommand :: String -> Command
toCommand cmd = (dirSteps, read @Int $ tail cmd)
  where
    dirSteps = case head cmd of
      'R' -> (1, 0)
      'L' -> (-1, 0)
      'U' -> (0, 1)
      'D' -> (0, -1)
      dir -> error $ "Bad direction: " ++ [dir]

toPointsMap :: [Command] -> PointsMap
toPointsMap = Map.fromListWith const . getPoints ((0, 0), 0)
  where
    getPoints _ [] = []
    getPoints ((currX, currY), currSteps) (cmd:cmds) =
      let ((dx, dy), steps) = cmd
          points = flip map [1..steps] $ \step ->
            let stepX = currX + dx * step
                stepY = currY + dy * step
            in ((stepX, stepY), currSteps + step)
      in points ++ getPoints (last points) cmds
