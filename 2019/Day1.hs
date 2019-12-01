{- stack script
    --resolver lts-14.12
-}

{-# LANGUAGE TypeApplications #-}

type Input = [Int]

main :: IO ()
main = do
  input <- map (read @Int) . lines <$> readFile "Day1.txt"
  print $ part1 input
  print $ part2 input

part1 :: Input -> Int
part1 = sum . map getFuel

part2 :: Input -> Int
part2 = sum . map getFuelIncludingFuel
  where
    getFuelIncludingFuel mass =
      let fuel = getFuel mass
      in if fuel <= 0
        then 0
        else fuel + getFuelIncludingFuel fuel

getFuel :: Int -> Int
getFuel mass = floor (fromIntegral mass / 3) - 2
