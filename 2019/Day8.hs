{- stack script
    --resolver lts-14.12
-}

{-# OPTIONS_GHC -Wall -Werror #-}

import Control.Monad (msum)
import Data.Char (digitToInt)
import Data.List (minimumBy, transpose)
import Data.List.Extra (chunksOf)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

main :: IO ()
main = do
  input <- map digitToInt . head . lines <$> readFile "Day8.txt"

  print $ part1 input
  print $ part2 input

part1 :: [Int] -> Int
part1 = checksum . getLayerWithLeastZeros . getLayers
  where
    countDigits n = length . filter (== n)
    getLayerWithLeastZeros = minimumBy (comparing (countDigits 0))
    checksum layer = countDigits 1 layer * countDigits 2 layer

part2 :: [Int] -> Image
part2 = parseImage . map (map decodeColor) . getLayers

{- Layer utilities -}

getLayers :: [Int] -> [[Int]]
getLayers = chunksOf layerLength
  where
    layerLength = layerWidth * layerHeight

layerWidth, layerHeight :: Int
layerWidth = 25
layerHeight = 6

{- Color -}

data Color = White | Black

decodeColor :: Int -> Maybe Color
decodeColor 0 = Just Black
decodeColor 1 = Just White
decodeColor 2 = Nothing
decodeColor n = error $ "Invalid color: " ++ show n

{- Image -}

newtype Image = Image { unImage :: [[Color]] }

parseImage :: [[Maybe Color]] -> Image
parseImage = Image . chunksOf layerWidth . map (fromJust . msum) . transpose

instance Show Image where
  show = unlines . map (map showColor) . unImage
    where
      showColor White = 'X'
      showColor Black = ' '
