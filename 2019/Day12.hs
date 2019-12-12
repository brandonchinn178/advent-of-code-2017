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
  input <- map readPosition . lines <$> readFile "Day12.txt"

  print $ part1 input

part1 :: [Position] -> Int
part1 = getSystemEnergy . (!! 1000) . iterate updateSystem . map (, [0, 0, 0])

{- System -}

-- Guaranteed to have three elements
type Velocity = [Int]

type System = [(Position, Velocity)]

getPositions :: System -> [Position]
getPositions = map fst

updateSystem :: System -> System
updateSystem = applyMovement . applyGravity
  where
    applyGravity :: System -> System
    applyGravity = mapWithOthers $ \(currPosition, currVelocity) others ->
      (currPosition, foldl (updateVelocity currPosition) currVelocity (getPositions others))

    updateVelocity :: Position -> Velocity -> Position -> Velocity
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
    getEnergy :: Position -> Velocity -> Int
    getEnergy = (*) `on` (sum . map abs)

{- Position -}

-- Guaranteed to have three elements
type Position = [Int]

readPosition :: String -> Position
readPosition = map parseNum . splitOn ", " . stripBrackets
  where
    stripBrackets = dropWhileEnd (== '>') . dropWhile (== '<')
    parseNum = read @Int . last . splitOn "="

{- Utilities -}

-- | Return a list with each element in the list paired with all the other
-- elements in the list.
--
-- mapWithOthers f [1,2,3] == [f 1 [2, 3], f 2 [1, 3], f 3 [1, 2]]
mapWithOthers :: (a -> [a] -> b) -> [a] -> [b]
mapWithOthers f = go []
  where
    go _ [] = []
    go past (x:xs) = f x (past ++ xs) : go (past ++ [x]) xs
