{- stack script
    --resolver lts-14.12
-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (uncons)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  program <- readProgram <$> readFile "Day13.txt"

  print $ part1 program

part1 :: Program -> Int
part1 = length . filter ((== Block) . fst) . parseOutputs . runProgram []

type Point = (Integer, Integer)
type Tile = (TileType, Point)

parseOutputs :: [Integer] -> [Tile]
parseOutputs (x:y:tileId:rest) = (toTileType tileId, (x, y)) : parseOutputs rest
parseOutputs [] = []
parseOutputs outputs = error $ "Outputs ended unexpectedly: " ++ show outputs

data TileType
  = Empty
  | Wall
  | Block
  | Paddle
  | Ball
  deriving (Eq,Show,Enum)

toTileType :: Integer -> TileType
toTileType = toEnum . fromIntegral

{------------------------------------------------------------------------------
|                             IntCode Program                                 |
------------------------------------------------------------------------------}

type Program = Map Integer Integer

data ProgramState = ProgramState
  { program      :: Program
  , position     :: Address
  , relativeBase :: Address
  , inputs       :: [Integer]
  } deriving (Show)

readProgram :: String -> Program
readProgram = Map.fromList . zip [0..] . map (read @Integer) . splitOn ","

runProgram :: [Integer] -> Program -> [Integer]
runProgram inputs program = execProgram ProgramState
  { position = 0
  , relativeBase = 0
  , ..
  }

execProgram :: ProgramState -> [Integer]
execProgram state =
  case getCommand (program state) (position state) of
    ADD p1 p2 out -> execBinOp (+) p1 p2 out
    MULTIPLY p1 p2 out -> execBinOp (*) p1 p2 out
    INPUT out ->
      let ProgramState{..} = state
          (input, rest) = fromMaybe
            (error $ "Ran out of inputs for the INPUT command at position: " ++ show position)
            (uncons inputs)
      in execProgram
        . bumpPosition 2
        . updateProgram out input
        $ state { inputs = rest }
    OUTPUT p1 -> resolve p1 : (execProgram . bumpPosition 2 $ state)
    JUMP_TRUE p1 p2 -> execJump (/= 0) p1 p2
    JUMP_FALSE p1 p2 -> execJump (== 0) p1 p2
    COMPARE_LT p1 p2 out -> execComp (<) p1 p2 out
    COMPARE_EQ p1 p2 out -> execComp (==) p1 p2 out
    ADJUST_BASE p1 -> execProgram . bumpPosition 2 . adjustBase (resolve p1) $ state
    HALT -> []
  where
    -- executors
    execBinOp f p1 p2 out =
      let result = resolveBinOp f p1 p2
      in execProgram . bumpPosition 4 . updateProgram out result $ state
    execJump f p1 p2 =
      let updatePosition = maybe (bumpPosition 3) setPosition $ resolveJump f p1 p2
      in execProgram . updatePosition $ state
    execComp f p1 p2 out =
      let result = resolveComp f p1 p2
      in execProgram . bumpPosition 4 . updateProgram out result $ state

    -- resolvers
    resolve = resolveParam (program state) (relativeBase state)
    resolveBinOp f p1 p2 = resolve p1 `f` resolve p2
    resolveJump f p1 p2 = if f (resolve p1) then Just (resolve p2) else Nothing
    resolveComp f p1 p2 = if resolve p1 `f` resolve p2 then 1 else 0

    -- state updaters
    setPosition pos s = s { position = pos }
    bumpPosition n s = s { position = position s + n }
    updateProgram out result s =
      let addressOut = resolveAddressParam (relativeBase s) out
      in s { program = Map.insert addressOut result (program s) }
    adjustBase offset s = s { relativeBase = relativeBase s + offset }

{- Parameters -}

type Address = Integer
type Value = Integer

data Parameter
  = ADDRESS AddressParameter
  | VALUE Integer
  deriving (Show)

data AddressParameter
  = ADDRESS_ABS Integer
  | ADDRESS_REL Integer
  deriving (Show)

resolveAddressParam :: Address -> AddressParameter -> Address
resolveAddressParam relativeBase = \case
  ADDRESS_ABS address -> address
  ADDRESS_REL offset -> relativeBase + offset

resolveParam :: Program -> Address -> Parameter -> Value
resolveParam program relativeBase = \case
  ADDRESS address -> program ! resolveAddressParam relativeBase address
  VALUE value -> value

{- Commands -}

data Command
  = ADD Parameter Parameter AddressParameter
  | MULTIPLY Parameter Parameter AddressParameter
  | INPUT AddressParameter
  | OUTPUT Parameter
  | JUMP_TRUE Parameter Parameter
  | JUMP_FALSE Parameter Parameter
  | COMPARE_LT Parameter Parameter AddressParameter
  | COMPARE_EQ Parameter Parameter AddressParameter
  | ADJUST_BASE Parameter
  | HALT
  deriving (Show)

getCommand :: Program -> Address -> Command
getCommand program position =
  case opcode of
    1 -> buildBinOp ADD
    2 -> buildBinOp MULTIPLY
    3 -> INPUT (addressParameter 0)
    4 -> OUTPUT (parseParameter 0)
    5 -> buildJump JUMP_TRUE
    6 -> buildJump JUMP_FALSE
    7 -> buildComp COMPARE_LT
    8 -> buildComp COMPARE_EQ
    9 -> ADJUST_BASE (parseParameter 0)
    99 -> HALT
    _ -> error $ "Invalid opcode: " ++ show opcode
  where
    (modes, opcode) = (program ! position) `divMod` 100

    addressParameter digit =
      case parseParameter digit of
        ADDRESS address -> address
        VALUE _ -> error "Invalid address parameter"
    parseParameter digit =
      let parameter = program ! (position + digit + 1)
      in case getDigit digit modes of
        0 -> ADDRESS $ ADDRESS_ABS parameter
        1 -> VALUE parameter
        2 -> ADDRESS $ ADDRESS_REL parameter
        mode -> error $ "Invalid parameter mode: " ++ show mode

    buildBinOp f = f (parseParameter 0) (parseParameter 1) (addressParameter 2)
    buildJump f = f (parseParameter 0) (parseParameter 1)
    buildComp f = f (parseParameter 0) (parseParameter 1) (addressParameter 2)

{- Utilities -}

(!) :: Program -> Integer -> Integer
program ! address = fromMaybe 0 $ Map.lookup address program

-- | Get the given digit from the number, starting from the least significant
-- digit.
getDigit :: Integer -> Integer -> Integer
getDigit digit x = (x `div` 10 ^ digit) `mod` 10
