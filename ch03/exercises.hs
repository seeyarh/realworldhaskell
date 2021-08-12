import Data.List

-- Ex 1, 2
myLen :: [a] -> Int
myLen [] = 0
myLen (_:xs) = 1 + myLen xs

-- Ex 3
myMean [] = 0.0
myMean xs = realToFrac (sum xs) / genericLength xs

-- Ex 4
palindrome xs = xs ++ reverse xs

-- Ex 5
isPalindrome [] = True
isPalindrome xs = xs == reverse xs

-- Ex 6
sortGT xs ys
  | length xs < length ys = LT
  | otherwise = GT

sortLenSublist :: [[a]] -> [[a]]
sortLenSublist = sortBy sortGT

-- Ex 7
myIntersperse :: a -> [[a]] -> [a]
myIntersperse _ [] = []
myIntersperse sep (x1:x2:xs) = x1 ++ [sep] ++ x2 ++ myIntersperse sep xs
myIntersperse sep [x] = x

-- Ex 8
data Tree a
  = Node a (Tree a) (Tree a)
  | Empty
  deriving (Show)

height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

-- Ex 9
data Point =
  Point
    { px :: Double
    , py :: Double
    }

data Vector =
  Vector
    { x :: Double
    , y :: Double
    }

fromPoints :: Point -> Point -> Vector
fromPoints p1 p2 = Vector {x = px p2 - px p1, y = py p2 - py p1}

dotProduct :: Vector -> Vector -> Double
dotProduct v1 v2 = x v1 * x v2 + y v1 + y v2

magnitude :: Vector -> Double
magnitude v = sqrt (x v * x v + y v * y v)

cosAngle :: Vector -> Vector -> Double
cosAngle v1 v2 = dotProduct v1 v2 / (magnitude v1 * magnitude v2)

angle :: Vector -> Vector -> Double
angle v1 v2 = acos (cosAngle v1 v2)

data Direction
  = ToLeft
  | ToRight
  | Straight

turn :: Point -> Point -> Point -> Direction
turn p1 p2 p3
  | theta == 0.0 = Straight
  | theta == pi = Straight
  | x v2 > x v1 = ToRight
  | otherwise = ToLeft
  where
    v1 = fromPoints p1 p2
    v2 = fromPoints p2 p3
    theta = angle v1 v2

-- from https://codereview.stackexchange.com/questions/206019/graham-scan-algorithm-in-haskell
convexHull :: (Num a, Ord a) => [(a, a)] -> [(a, a)]
convexHull points
  | length sorted <= 2 = sorted
  | otherwise = init (grahamScan sorted) ++ init (grahamScan $ reverse sorted)
  where
    sorted = map head $ group $ sort points

grahamScan :: (Num a, Ord a) => [(a, a)] -> [(a, a)]
grahamScan = foldr push []
  where
    push point stack = grahamEliminate (point : stack)

grahamEliminate :: (Num a, Ord a) => [(a, a)] -> [(a, a)]
grahamEliminate (p1:p2:p3:stack)
  | doubleArea p1 p2 p3 <= 0 = grahamEliminate (p1 : p3 : stack)
grahamEliminate stack = stack

doubleArea :: Num a => (a, a) -> (a, a) -> (a, a) -> a
doubleArea (x1, y1) (x2, y2) (x3, y3) =
  (x2 - x1) * (y3 - y1) * (y3 - y1) - (y2 - y1) * (x3 - x1)
