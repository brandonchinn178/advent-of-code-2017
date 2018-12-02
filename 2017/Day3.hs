{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Day3 where

-- | Helper that returns Just (sqrt x) if the given input is a perfect
-- square, or Nothing otherwise.
evenSqrt :: Int -> Maybe Int
evenSqrt x = if sqrtFrac == 0 then Just sqrtInt else Nothing
  where
    (sqrtInt, sqrtFrac) = properFraction $ sqrt $ fromIntegral x

-- The spiral is such that the bottom right corner of each layer
-- are successive odd squares.
--
-- 17  16  15  14  13
-- 18   5   4   3  12
-- 19   6   1   2  11
-- 20   7   8   9  10
-- 21  22  23 --> ...
type Spiral = [Int]

-- | Layer with 1 is layer 0, layer from 2-9 is layer 1, etc.
type Layer = Int

layerToWidth :: Layer -> Int
layerToWidth layer = 2 * layer + 1

widthToLayer :: Int -> Layer
widthToLayer width = (width - 1) `div` 2

-- | Get the lowest square in a layer (e.g. layer 1 -> 2).
getLayerMin :: Layer -> Int
getLayerMin layer = getLayerMax (layer - 1) + 1

-- | Get the highest square in a layer (e.g. layer 1 -> 9).
getLayerMax :: Layer -> Int
getLayerMax layer = truncate $ fromIntegral width ** 2
  where
    width = layerToWidth layer

-- | The side of a spiral.
data Side = R | T | L | B deriving (Eq, Show)

nextSide :: Side -> Side
nextSide = \case
  R -> T
  T -> L
  L -> B
  B -> R

-- | The corners of a layer.
data Corners = Corners
  { topLeft :: Int
  , topRight :: Int
  , bottomLeft :: Int
  , bottomRight :: Int
  } deriving (Eq, Show)

-- | Get the corners for the sides of the given layer.
getCorners :: Layer -> Corners
getCorners layer = Corners{..}
  where
    width = layerToWidth layer
    bottomRight = getLayerMax layer
    bottomLeft = bottomRight - width + 1
    topLeft = bottomLeft - width + 1
    topRight = topLeft - width + 1

-- | Get the boundaries of the given side as (exclusive, inclusive).
getSideBoundaries :: Layer -> Side -> (Int, Int)
getSideBoundaries layer = \case
  R -> (topRight - width + 1, topRight)
  T -> (topRight, topLeft)
  L -> (topLeft, bottomLeft)
  B -> (bottomLeft, bottomRight)
  where
    width = layerToWidth layer
    Corners{..} = getCorners layer

-- | Get the square in the middle of the given side.
getSideMiddle :: Layer -> Side -> Int
getSideMiddle layer side = (high + low) `div` 2
  where
    (low, high) = getSideBoundaries layer side
