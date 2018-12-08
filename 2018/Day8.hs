{- stack script
    --resolver lts-12.12
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

data Node = Node
  { children    :: [Node]
  , metadata    :: [Int]
  } deriving (Show)

main :: IO ()
main = do
  input <- parse . head . lines <$> readFile "Day8.txt"
  let example = fromData [2,3,0,3,10,11,12,1,1,0,1,99,2,1,1,2]
  -- print example
  -- print $ part1 example
  print $ part1 input
  -- print $ part2 example
  print $ part2 input

parse :: String -> Node
parse = fromData . map read . words

fromData :: [Int] -> Node
fromData input = case fromData' input of
  (node, []) -> node
  (_, rest) -> error $ "Not finished processing: " ++ show rest
  where
    fromData' = \case
      0:numMeta:rest ->
        let (metadata, rest') = splitAt numMeta rest
        in (Node{children = [], ..}, rest')
      numChildren:numMeta:rest ->
        let (children', rest') = (!! numChildren) $ iterate parseChildren ([], rest)
            children = reverse children'
            (metadata, rest'') = splitAt numMeta rest'
        in (Node{..}, rest'')
    parseChildren (acc, input) =
      let (child, rest) = fromData' input
      in (child:acc, rest)

part1 :: Node -> Int
part1 Node{..} = sum $ metadata ++ map part1 children

part2 :: Node -> Int
part2 node@Node{..} =
  if null children
    then part1 node
    else sum $ map getChild metadata
  where
    getChild i = if i > length children then 0 else part2 $ children !! (i - 1)
