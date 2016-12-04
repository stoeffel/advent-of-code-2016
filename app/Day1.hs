module Day1 where


import Data.Either
import Data.List
import Data.List.Split
import Safe


data Direction
  = L Int
  | R Int
  deriving (Show)


data CardinalDirection
  = N Int
  | E Int
  | S Int
  | W Int
  deriving (Show)

instance Eq CardinalDirection where
  N _ == N _ = True
  E _ == E _ = True
  S _ == S _ = True
  W _ == W _ = True
  _ == _ = False

instance Ord CardinalDirection where
  compare (E _) (N _) = GT
  compare (S _) (N _) = GT
  compare (W _) (N _) = GT
  compare (S _) (E _) = GT
  compare (W _) (E _) = GT
  compare (W _) (S _) = GT
  compare x y =
    if x == y then
      EQ
    else
      LT

type Blocks = Int
type Location = (Int, Int)


-- | returns the direction from a string
-- >>> directionFromString "L42"
-- Right (L 42)
-- >>> directionFromString "R3"
-- Right (R 3)
-- >>> directionFromString "Q3"
-- Left "Unknown direction!"
directionFromString :: String -> Either String Direction
directionFromString ('R':n) = R <$> parseInt "Invalid direction!" n
directionFromString ('L':n) = L <$> parseInt "Invalid direction!" n
directionFromString _ = Left "Unknown direction!"

parseInt :: String -> String -> Either String Int
parseInt e n =
  case readMay n :: Maybe Int of
    Just n -> Right n
    Nothing -> Left e


-- | parse directions
-- >>> parse "L3, R2"
-- Right [L 3,R 2]
-- >>> parse "L3, X2"
-- Left "Unknown direction!"
parse :: String -> Either String [Direction]
parse = sequence . map directionFromString . splitOn ", "


-- | turns directions into cardinal directions
-- >>> toCardinalDirections [L 3]
-- [W 3]
-- >>> toCardinalDirections [R 3]
-- [E 3]
-- >>> toCardinalDirections [R 3, L 3]
-- [E 3,N 3]
-- >>> toCardinalDirections [R 3, L 3, R 4]
-- [E 3,N 3,E 4]
-- >>> toCardinalDirections [R 1, R 1, R 1, R 1, L 1, L 1, L 1, L 1]
-- [E 1,S 1,W 1,N 1,W 1,S 1,E 1,N 1]
toCardinalDirections :: [Direction] -> [CardinalDirection]
toCardinalDirections dirs = reverse $ foldl toCardinalDirection [] dirs where
  toCardinalDirection :: [CardinalDirection] -> Direction -> [CardinalDirection]
  toCardinalDirection [] (R n) = [E n]
  toCardinalDirection [] (L n) = [W n]
  toCardinalDirection xs@(N _:_) (R n)= E n:xs
  toCardinalDirection xs@(E _:_) (R n)= S n:xs
  toCardinalDirection xs@(S _:_) (R n)= W n:xs
  toCardinalDirection xs@(W _:_) (R n)= N n:xs
  toCardinalDirection xs@(N _:_) (L n)= W n:xs
  toCardinalDirection xs@(E _:_) (L n)= N n:xs
  toCardinalDirection xs@(S _:_) (L n)= E n:xs
  toCardinalDirection xs@(W _:_) (L n)= S n:xs


-- | groups cardinal directions and sums the distance
-- >>> groupCardinalDirections [E 1,S 1,W 1,N 1,W 1,S 1,E 1,N 1]
-- [N 2,E 2,S 2,W 2]
groupCardinalDirections :: [CardinalDirection] -> [CardinalDirection]
groupCardinalDirections = map sumDirs . groupBy (==) . sort where
  sumDirs :: [CardinalDirection] -> CardinalDirection
  sumDirs [] = N 0
  sumDirs (N x:xs) = N $ x + (sum $ map getDistance xs)
  sumDirs (E x:xs) = E $ x + (sum $ map getDistance xs)
  sumDirs (S x:xs) = S $ x + (sum $ map getDistance xs)
  sumDirs (W x:xs) = W $ x + (sum $ map getDistance xs)

  getDistance :: CardinalDirection -> Int
  getDistance (N x) = x
  getDistance (E x) = x
  getDistance (S x) = x
  getDistance (W x) = x


-- | calculates the distance using a list of directions
-- >>> distance "R 2, L 3"
-- Right 5
-- >>> distance "R 2, R 2, R 2"
-- Right 2
-- >>> distance "R 5, L 5, R 5, R 3"
-- Right 12
-- >>> distance "R 5, L 5, R x, R 3"
-- Left "Invalid direction!"
-- >>> distance "R 5, L 5, R42, r 3"
-- Left "Unknown direction!"
distance :: String -> Either String Blocks
distance str =
  abs
  . blocks 0
  . groupCardinalDirections
  . toCardinalDirections
  <$> parse str


blocks :: Blocks -> [CardinalDirection] -> Blocks
blocks n [] = n
blocks n (N x:xs) = blocks (n + x) xs
blocks n (E x:xs) = blocks (n + x) xs
blocks n (S x:xs) = blocks (n - x) xs
blocks n (W x:xs) = blocks (n - x) xs


-- |
-- >>> toLocation []
-- [(0,0)]
-- >>> toLocation [W 3]
-- [(0,0),(-1,0),(-2,0),(-3,0)]
toLocation :: [CardinalDirection] -> [Location]
toLocation [] = [(0, 0)]
toLocation xs = reverse $ foldl go [(0, 0), (0, 0)] xs where
  go :: [Location] -> CardinalDirection -> [Location]
  go (x:xs) (N n) = eachStep n (\(a, b) -> (a, b + 1)) x ++ xs
  go (x:xs) (S n) = eachStep n (\(a, b) -> (a, b - 1)) x ++ xs
  go (x:xs) (E n) = eachStep n (\(a, b) -> (a + 1, b)) x ++ xs
  go (x:xs) (W n) = eachStep n (\(a, b) -> (a - 1, b)) x ++ xs

  eachStep :: Int -> (Location -> Location) -> Location -> [Location]
  eachStep n mapLoc =
    reverse . take n . drop 1 . iterate mapLoc


-- |
-- >>> firstLocationVisitedTwice "R8, R4, R4, R8"
-- Right 4
firstLocationVisitedTwice :: String -> Either String Blocks
firstLocationVisitedTwice str =
  findFirstDup []
  . toLocation
  . toCardinalDirections
  <$> parse str
  >>= distanceToMaybeLocation


findFirstDup :: [Location] -> [Location] -> Maybe Location
findFirstDup [] [] = Nothing
findFirstDup ys [] = Nothing
findFirstDup [] (x:xs) = findFirstDup [x] xs
findFirstDup ys (x:xs) =
  if elem x ys then
    Just x
  else
    findFirstDup (x:ys) xs


distanceToMaybeLocation :: Maybe Location -> Either String Blocks
distanceToMaybeLocation Nothing = Left "No duplicate location!"
distanceToMaybeLocation (Just (x, y)) = Right $ abs (x + y)
