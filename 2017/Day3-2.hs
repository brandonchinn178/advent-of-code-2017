{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Day3
import Utils

type Position = (Int, Int) -- The position of a square; (0, 0) is square 1
type Square = Int -- The square number; e.g. 1, 2, 3, 4
type Value = Integer -- The value in a position

isCorner :: Position -> Bool
isCorner (x, y) = abs x == abs y

isLast :: Position -> Bool
isLast (x, y) = x >= 0 && x == -y

nextPosition :: Position -> Position
nextPosition p = (x + dx, y + dy)
  where
    (x, y) = p
    currSide = positionToSide p
    nextSide' = if isCorner p then nextSide currSide else currSide
    dx = case nextSide' of
      R -> if isLast p then 1 else 0
      T -> -1
      L -> 0
      B -> 1
    dy = case nextSide' of
      R -> if isLast p then 0 else 1
      T -> 0
      L -> -1
      B -> 0

positionToLayer :: Position -> Layer
positionToLayer (x, y) = maximum $ map abs [x, y]

positionToSide :: Position -> Side
positionToSide p
  | isLast p = B
  | x == layer = R
  | y == layer = T
  | x == -layer = L
  | otherwise = B
  where
    (x, y) = p
    layer = positionToLayer p

-- | Position to number of squares from the bottom right corner of the
-- previous layer.
positionToOffset :: Position -> Int
positionToOffset p = ((width - 1) * sideOffset) + offset
  where
    layer = positionToLayer p
    width = layerToWidth layer
    side = positionToSide p
    sideOffset = case side of
      R -> 0
      T -> 1
      L -> 2
      B -> 3
    (x, y) = p
    offsetFromHalf = case side of
      R -> y
      T -> -x
      L -> -y
      B -> x
    offset = layer + offsetFromHalf

positionToSquare :: Position -> Square
positionToSquare p = getLayerMax prevLayer + positionToOffset p
  where
    prevLayer = positionToLayer p - 1

positionToValue :: [Value] -> Position -> Value
positionToValue prev position = if square < length prev then prev !! square else 0
  where
    square = positionToSquare position - 1

getNext :: (Value, [Value], Position) -> (Value, [Value], Position)
getNext (_, prev, position) = (value, prev ++ [value], nextPosition position)
  where
    (x, y) = position
    value = sum
      [ positionToValue prev (x + dx, y + dy)
      | dx <- [-1..1], dy <- [-1..1], (dx /= 0 || dy /= 0)
      ]

values :: [Value]
values = map fst' $ iterate getNext (1, [1], nextPosition (0, 0))
  where
    fst' (x, _, _) = x

-- | The first value written that is larger than the given input.
firstLargerThan :: Value -> Value
firstLargerThan input = head $ filter (> input) values

main :: IO ()
main = part 2 $ do
  check "positionToLayer" positionToLayer
    [ ((0, 0), 0)
    , ((1, 0), 1)
    , ((1, 1), 1)
    , ((0, 1), 1)
    , ((-1, 1), 1)
    , ((-1, 0), 1)
    , ((-1, -1), 1)
    , ((0, -1), 1)
    , ((1, -1), 1)
    , ((2, -1), 2)
    , ((2, 0), 2)
    , ((2, 1), 2)
    , ((2, 2), 2)
    , ((1, 2), 2)
    , ((2, -2), 2)
    ]
  check "positionToOffset" positionToOffset
    [ ((0, 0), 0)
    , ((1, 0), 1)
    , ((1, 1), 2)
    , ((0, 1), 3)
    , ((-1, 1), 4)
    , ((-1, 0), 5)
    , ((-1, -1), 6)
    , ((0, -1), 7)
    , ((1, -1), 8)
    , ((2, -1), 1)
    , ((2, 0), 2)
    , ((2, 1), 3)
    , ((2, 2), 4)
    , ((1, 2), 5)
    , ((0, 2), 6)
    , ((-1, 2), 7)
    , ((-2, 2), 8)
    , ((-2, 1), 9)
    , ((-2, 0), 10)
    , ((-2, -1), 11)
    , ((-2, -2), 12)
    , ((-1, -2), 13)
    , ((0, -2), 14)
    , ((1, -2), 15)
    , ((2, -2), 16)
    , ((3, -2), 1)
    ]
  check "positionToSquare" positionToSquare
    [ ((0, 0), 1)
    , ((1, 0), 2)
    , ((1, 1), 3)
    , ((0, 1), 4)
    , ((-1, 1), 5)
    , ((-1, 0), 6)
    , ((-1, -1), 7)
    , ((0, -1), 8)
    , ((1, -1), 9)
    , ((2, -1), 10)
    , ((2, 0), 11)
    , ((2, 1), 12)
    , ((2, 2), 13)
    , ((1, 2), 14)
    , ((0, 2), 15)
    , ((-1, 2), 16)
    , ((-2, 2), 17)
    , ((-2, 1), 18)
    , ((-2, 0), 19)
    , ((-2, -1), 20)
    , ((-2, -2), 21)
    , ((-1, -2), 22)
    , ((0, -2), 23)
    , ((1, -2), 24)
    , ((2, -2), 25)
    ]
  check "firstLargerThan" firstLargerThan
    [ (1, 2)
    , (12, 23)
    , (23, 25)
    , (800, 806)
    ]
  solve $ firstLargerThan 325489
