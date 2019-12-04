import Control.Applicative
import Data.List
import Data.List.Split
import qualified Data.Set as S
import qualified Data.Map as M

data Loc = Loc Int Int deriving (Show, Read, Eq, Ord)
data Direction = U | R | D | L deriving (Show, Read)
data Operation = Operation Direction Int deriving (Show, Read)

parseProgram :: String -> [Operation]
parseProgram = fmap parseOperation . splitOn ","
  where
    parseOperation (dir:rest) = Operation (read [dir]) (read rest)

newLoc :: Loc -> Direction -> Loc
newLoc (Loc x y) U = Loc x (y + 1)
newLoc (Loc x y) R = Loc (x + 1) y
newLoc (Loc x y) D = Loc x (y - 1)
newLoc (Loc x y) L = Loc (x - 1) y

getLocsForProgram :: [Operation] -> [Loc]
getLocsForProgram = scanl' newLoc (Loc 0 0) . concat . fmap getMoves
  where
    getMoves (Operation dir count) = replicate count dir

main :: IO ()
main = do
  wire1 <- getLocsForProgram . parseProgram <$> getLine
  wire2 <- getLocsForProgram . parseProgram <$> getLine

  let intersections = filter (flip S.member (S.fromList wire1)) wire2

  print . closestBy distance $ intersections

  let stepsForLoc1 = M.fromList $ zip wire1 [0..] :: M.Map Loc Int
  let stepsForLoc2 = M.fromList $ zip wire2 [0..] :: M.Map Loc Int

  print . closestBy (liftA2 (+) <$> flip M.lookup stepsForLoc1 <*> flip M.lookup stepsForLoc2) $ intersections

  where
    closestBy f = head . drop 1 . sort . fmap f
    distance (Loc x y) = abs x + abs y