import Data.List
import Data.List.Split
import Debug.Trace
import Data.Foldable

type Layer = [Int]
type Image = [Layer]

parseInput :: String -> Image
parseInput = chunksOf (25 * 6) . fmap (read . (:[]))

main :: IO ()
main = do
  image <- parseInput . head . lines <$> readFile "input.txt"

  let lowestZeroes = minimumBy (\a b -> compare (countOf 0 a) (countOf 0 b)) $ image

  print $ countOf 1 lowestZeroes * countOf 2 lowestZeroes

  let mergedImage = fmap (foldr overlay 0) . transpose $ image

  mapM_ putStrLn . chunksOf 25 . fmap friendly $ mergedImage

  where
    countOf n = length . filter (== n)

    overlay :: Int -> Int -> Int
    overlay 0 _ = 0
    overlay 1 _ = 1
    overlay 2 x = x

    friendly 0 = ' '
    friendly 1 = 'X'