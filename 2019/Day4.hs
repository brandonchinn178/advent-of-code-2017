{- stack script
    --resolver lts-14.12
-}

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeApplications #-}

import Data.List (group)

main :: IO ()
main = do
  let low = 387638
      high = 919123

  print $ part1 low high
  print $ part2 low high

part1 :: Int -> Int -> Int
part1 = findInRangeWhere (meetsCriteria . splitDigits)
  where
    meetsCriteria digits =
      all (uncurry (<=)) (toNeighbors digits) &&
      any (uncurry (==)) (toNeighbors digits)

part2 :: Int -> Int -> Int
part2 = findInRangeWhere (meetsCriteria . splitDigits)
  where
    meetsCriteria digits =
      all (uncurry (<=)) (toNeighbors digits) &&
      ((2 `elem`) . map length . group) digits

findInRangeWhere :: (Int -> Bool) -> Int -> Int -> Int
findInRangeWhere f low high = length $ filter f [low..high]

-- | Split the given number into digits, where the 0-th element is the
-- most significant digit.
--
-- Every element in the resulting list is guaranteed to be 0-9.
splitDigits :: Int -> [Int]
splitDigits x = reverse . take numDigits . map (`mod` 10) . iterate (`div` 10) $ x
  where
    numDigits = 1 + (floor . logBase @Double 10 . fromIntegral $ x)

toNeighbors :: [a] -> [(a, a)]
toNeighbors xs = zip xs $ tail xs
