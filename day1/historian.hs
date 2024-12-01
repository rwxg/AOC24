import Data.List
import Data.Semigroup
import System.Environment (getArgs)

parseFile :: String -> ([Int],[Int])
parseFile content =
  let pairs = map parseLine $ lines content
  in (map fst pairs, map snd pairs)

parseLine :: String -> (Int, Int)
parseLine line =
  let [a, b] = map read (words line)
  in (a, b)

-- minFst :: [(Int, Int)] -> Int
-- minFst [] = error "Empty list"
-- minFst xs = minimum $ map fst xs
-- 
-- minSnd :: [(Int, Int)] -> Int
-- minSnd [] = error "Empty list"
-- minSnd xs = minimum $ map snd xs

minElement :: (Ord a) => [a] -> (a, [a])
minElement xs = (minEl, rest)
  where
    minEl = minimum xs
    rest = delete minEl xs

pairSmallest :: (Ord a) => [a] -> [a] -> [(a,a)]
pairSmallest [] _ = []
pairSmallest _ [] = []
pairSmallest xs ys = (xMin, yMin) : pairSmallest xsRest ysRest
  where
    (xMin, xsRest) = minElement xs
    (yMin, ysRest) = minElement ys

pairDiff :: (Int,Int) -> Int
pairDiff p = abs $ uncurry (-) p

cumulativeDifference :: [Int] -> [Int] -> Int
cumulativeDifference [] [] = 0
cumulativeDifference xs ys = sum $ pairDiff <$> pairSmallest xs ys

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

similarityScore :: [Int] -> [Int] -> Int
similarityScore [] [] = 0
similarityScore xs ys = sum scores
  where
    scores = [ x * c | x <- xs,
                       let c = count x ys ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- readFile filePath
      let (leftValues, rightValues) = parseFile content
      -- putStrLn "Left values:"
      -- print leftValues
      -- putStrLn "Right values:"
      -- print rightValues
      let cumDiff = cumulativeDifference leftValues rightValues
      let simScore = similarityScore leftValues rightValues
      print cumDiff
      putStrLn "Similarity:"
      print simScore
    _ -> putStrLn "Usage: ./program <file>"

