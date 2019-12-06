{- stack script
    --resolver lts-14.12
-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.RWS.Strict (RWS, execRWS)
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (get, put)
import Control.Monad.Writer.Strict (tell)
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as IntMap
import Data.List.Split (splitOn)

type Program = IntMap Int

-- A program *reads* a single number as input, *writes* outputted numbers, and
-- has *state* containing the program and current position.
type ProgramM = RWS Int [Int] Program

main :: IO ()
main = do
  program <- IntMap.fromList
    . zip [0..]
    . map (read @Int)
    . splitOn ","
    <$> readFile "Day5.txt"

  print $ part1 program
  print $ part2 program

part1 :: Program -> Either [Int] Int
part1 = runProgram 1

part2 :: Program -> Either [Int] Int
part2 = runProgram 5

-- | Returns either the list of outputs if an error occurred, or the diagnostic
-- code
runProgram :: Int -> Program -> Either [Int] Int
runProgram input program =
  let (_, outputs) = execRWS (execProgram 0) input program
  in if
    | null outputs -> Left []
    | any (/= 0) (init outputs) -> Left outputs
    | otherwise -> Right $ last outputs

execProgram :: Address -> ProgramM ()
execProgram position = do
  program <- get
  case getCommand program position of
    ADD p1 p2 out -> do
      let result = resolve program p1 + resolve program p2
      put $ IntMap.insert out result program
      execProgram $ position + 4
    MULTIPLY p1 p2 out -> do
      let result = resolve program p1 * resolve program p2
      put $ IntMap.insert out result program
      execProgram $ position + 4
    INPUT out -> do
      input <- ask
      put $ IntMap.insert out input program
      execProgram $ position + 2
    OUTPUT p1 -> do
      tell [resolve program p1]
      execProgram $ position + 2
    JUMP_TRUE p1 p2 ->
      if resolve program p1 /= 0
        then execProgram $ resolve program p2
        else execProgram $ position + 3
    JUMP_FALSE p1 p2 ->
      if resolve program p1 == 0
        then execProgram $ resolve program p2
        else execProgram $ position + 3
    COMPARE_LT p1 p2 out -> do
      let result = if resolve program p1 < resolve program p2 then 1 else 0
      put $ IntMap.insert out result program
      execProgram $ position + 4
    COMPARE_EQ p1 p2 out -> do
      let result = if resolve program p1 == resolve program p2 then 1 else 0
      put $ IntMap.insert out result program
      execProgram $ position + 4
    HALT -> return ()

{- Parameters -}

type Address = Int
type Value = Int

data Parameter
  = ADDRESS Address
  | VALUE Value

resolve :: Program -> Parameter -> Value
resolve program = \case
  ADDRESS address -> program ! address
  VALUE value -> value

{- Commands -}

data Command
  = ADD Parameter Parameter Address
  | MULTIPLY Parameter Parameter Address
  | INPUT Address
  | OUTPUT Parameter
  | JUMP_TRUE Parameter Parameter
  | JUMP_FALSE Parameter Parameter
  | COMPARE_LT Parameter Parameter Address
  | COMPARE_EQ Parameter Parameter Address
  | HALT

getCommand :: Program -> Address -> Command
getCommand program position =
  let (modes, opcode) = (program ! position) `divMod` 100
  in case opcode of
    1 -> ADD
      (parseParameter modes 0)
      (parseParameter modes 1)
      (rawParameter 2)
    2 -> MULTIPLY
      (parseParameter modes 0)
      (parseParameter modes 1)
      (rawParameter 2)
    3 -> INPUT (rawParameter 0)
    4 -> OUTPUT (parseParameter modes 0)
    5 -> JUMP_TRUE
      (parseParameter modes 0)
      (parseParameter modes 1)
    6 -> JUMP_FALSE
      (parseParameter modes 0)
      (parseParameter modes 1)
    7 -> COMPARE_LT
      (parseParameter modes 0)
      (parseParameter modes 1)
      (rawParameter 2)
    8 -> COMPARE_EQ
      (parseParameter modes 0)
      (parseParameter modes 1)
      (rawParameter 2)
    99 -> HALT
    _ -> error $ "Invalid opcode: " ++ show opcode
  where
    rawParameter digit = program ! (position + digit + 1)
    parseParameter modes digit =
      let parameter = rawParameter digit
      in case getDigit digit modes of
        0 -> ADDRESS parameter
        1 -> VALUE parameter
        mode -> error $ "Invalid parameter mode: " ++ show mode

-- | Get the given digit from the number, starting from the least significant
-- digit.
getDigit :: Int -> Int -> Int
getDigit digit x = (x `div` 10 ^ digit) `mod` 10
