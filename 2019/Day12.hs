{- stack script
    --resolver lts-14.12
-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Data.Function (on)
import Data.List (dropWhileEnd)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  input <- map readCoordinate . lines <$> readFile "Day12.txt"

  print $ part1 input
  print $ part2
    [ [-1, 0, 2]
    , [2, -10, -7]
    , [4, -8, 8]
    , [3, 5, -1]
    ]
  print $ part2
    [ [-8, -10, 0]
    , [5, 5, 10]
    , [2, -7, 3]
    , [9, -8, -3]
    ]

part1 :: [Coordinate] -> Int
part1 = getSystemEnergy . (!! 1000) . iterate updateSystem . map (, [0, 0, 0])

part2 :: [Coordinate] -> Integer
part2 = go Set.empty . iterate updateSystem . map (, [0, 0, 0])
  where
    go _ [] = error "unreachable"
    go seen (curr:rest) = if curr `Set.member` seen
      then fromIntegral $ length seen
      else go (Set.insert curr seen) rest

{- System -}

type PerMoon a = [a]
type PerAxis a = [a] -- Exactly 3 elements

type Velocity = Int
type Vector = PerAxis Velocity

type System = PerMoon (Coordinate, Vector)

getCoordinates :: System -> PerMoon Coordinate
getCoordinates = map fst

updateSystem :: System -> System
updateSystem = applyMovement . applyGravity
  where
    applyGravity :: System -> System
    applyGravity system =
      [ (position, foldl (updateVelocity position) velocity (getCoordinates system))
      | (position, velocity) <- system
      ]

    updateVelocity :: Coordinate -> Vector -> Coordinate -> Vector
    updateVelocity = zipWith3 $ \currPosition currVelocity otherPosition ->
      let delta = case compare currPosition otherPosition of
            LT -> 1
            EQ -> 0
            GT -> -1
      in currVelocity + delta

    applyMovement :: System -> System
    applyMovement = map $ \(position, velocity) -> (zipWith (+) position velocity, velocity)

getSystemEnergy :: System -> Int
getSystemEnergy = sum . map (uncurry getEnergy)
  where
    getEnergy :: Coordinate -> Vector -> Int
    getEnergy = (*) `on` (sum . map abs)

{- Position -}

type Position = Int
type Coordinate = PerAxis Position

readCoordinate :: String -> Coordinate
readCoordinate = map parseNum . splitOn ", " . stripBrackets
  where
    stripBrackets = dropWhileEnd (== '>') . dropWhile (== '<')
    parseNum = read @Int . last . splitOn "="
