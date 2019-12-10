{- stack script
    --resolver lts-14.12
-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE LambdaCase #-}

import Control.Arrow ((&&&))
import Data.List (maximumBy, sortBy, transpose)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Ord (comparing)

main :: IO ()
main = do
  points <- readMap <$> readFile "Day10.txt"

  print $ part1 points
  print $ part2 points

part1 :: [Point] -> (Point, Int)
part1 points = maximumBy (comparing snd) $ map (id &&& countViewableAsteroids) points
  where
    countViewableAsteroids = length . flip getLinesOfSight points

part2 :: [Point] -> Point
part2 points = toVaporizeOrder point points !! 199
  where
    point = fst $ part1 points

-- | Group points by their angle to the given point.
--
-- Points are ordered by distance, with closest point at head of list.
getLinesOfSight :: Point -> [Point] -> Map Double [Point]
getLinesOfSight point = Map.map sortByDist . categorize (getAngle point) . filter (/= point)
  where
    sortByDist = sortBy $ comparing (getManhattanDist point)

toVaporizeOrder :: Point -> [Point] -> [Point]
toVaporizeOrder point = concat . transpose . orderPointsClockwise . getLinesOfSight point . filter (/= point)
  where
    orderPointsClockwise :: Map Double [Point] -> [[Point]]
    orderPointsClockwise = map snd . Map.toDescList . Map.mapKeys (\x -> if x > pi/2 then x - 2 * pi else x)

{- Point -}

type Point = (Int, Int)

readMap :: String -> [Point]
readMap = concat . zipWith readRow [0..] . lines
  where
    readRow j =
      let mkPoint i = \case
            '.' -> Nothing
            '#' -> Just (i, j)
            c -> error $ "Bad character: " ++ show c
      in catMaybes . zipWith mkPoint [0..]

-- range [-pi,pi], where pi/2 points up on the map
getAngle :: Point -> Point -> Double
getAngle (x1, y1) (x2, y2) = atan2 dy dx
  where
    dy = fromIntegral $ negate $ y2 - y1
    dx = fromIntegral $ x2 - x1

getManhattanDist :: Point -> Point -> Int
getManhattanDist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

{- Utilities -}

categorize :: Ord k => (a -> k) -> [a] -> Map k [a]
categorize getKey = Map.fromListWith (++) . map (getKey &&& (:[]))
