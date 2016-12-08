{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
module Day1 where


import Data.List
import Data.List.Split
import Data.Monoid (Sum(..))
import Safe

type Direction = DirectionF Int

data DirectionF a
  = L a
  | R a
  deriving (Show, Functor)

type CardinalDirection = CardinalDirectionF Int

data CardinalDirectionF a
  = N a
  | E a
  | S a
  | W a
  deriving (Show, Functor)

instance Enum (CardinalDirectionF Int) where
  toEnum x =
    case mod x 4 of
      0 -> N 0
      1 -> E 0
      2 -> S 0
      _ -> W 0
  fromEnum x =
    case x of
      N _ -> 0
      E _ -> 1
      S _ -> 2
      W _ -> 3

instance Eq (CardinalDirectionF a) where
  N _ == N _ = True
  E _ == E _ = True
  S _ == S _ = True
  W _ == W _ = True
  _ == _ = False

instance Ord (CardinalDirectionF a) where
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

parseInt :: Read b => a -> String -> Either a b
parseInt e =
  maybe (Left e) Right . readMay

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
toCardinalDirections :: Foldable t => t Direction -> [CardinalDirection]
toCardinalDirections dirs = reverse $ foldl toCardinalDirection [] dirs where
  toCardinalDirection :: [CardinalDirection] -> Direction -> [CardinalDirection]
  toCardinalDirection [] (R n) = [E n]
  toCardinalDirection [] (L n) = [W n]
  toCardinalDirection xs@(c:_) d = toCardinalDirection' d c : xs

toCardinalDirection' :: DirectionF b -> CardinalDirectionF Int -> CardinalDirectionF b
toCardinalDirection' (R n) x = n <$ succ x
toCardinalDirection' (L n) x = n <$ pred x

-- | groups cardinal directions and sums the distance
-- >>> groupCardinalDirections [E 1,S 1,W 1,N 1,W 1,S 1,E 1,N 1]
-- [N 2,E 2,S 2,W 2]
groupCardinalDirections :: [CardinalDirection] -> [CardinalDirection]
groupCardinalDirections = map (fmap getSum . sumDirs) . groupBy (==) . sort

sumDirs :: Num a => [CardinalDirectionF a] -> CardinalDirectionF (Sum a)
sumDirs [] = N 0
sumDirs (N x:xs) = N $ Sum x + (foldMap getDistance xs)
sumDirs (E x:xs) = E $ Sum x + (foldMap getDistance xs)
sumDirs (S x:xs) = S $ Sum x + (foldMap getDistance xs)
sumDirs (W x:xs) = W $ Sum x + (foldMap getDistance xs)

getDistance :: CardinalDirectionF a -> Sum a
getDistance (N x) = Sum x
getDistance (E x) = Sum x
getDistance (S x) = Sum x
getDistance (W x) = Sum x


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
toLocation xs' = reverse $ foldl go [(0, 0), (0, 0)] xs' where
  go :: [Location] -> CardinalDirection -> [Location]
  go (x:xs) (N n) = eachStep n (\(a, b) -> (a, b + 1)) x ++ xs
  go (x:xs) (S n) = eachStep n (\(a, b) -> (a, b - 1)) x ++ xs
  go (x:xs) (E n) = eachStep n (\(a, b) -> (a + 1, b)) x ++ xs
  go (x:xs) (W n) = eachStep n (\(a, b) -> (a - 1, b)) x ++ xs
  go _      _     = []

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
findFirstDup _  [] = Nothing
findFirstDup [] (x:xs) = findFirstDup [x] xs
findFirstDup ys (x:xs) =
  if elem x ys then
    Just x
  else
    findFirstDup (x:ys) xs


distanceToMaybeLocation :: Maybe Location -> Either String Blocks
distanceToMaybeLocation Nothing = Left "No duplicate location!"
distanceToMaybeLocation (Just (x, y)) = Right $ abs (x + y)
