import Data.Char (digitToInt, isDigit, isSpace)
import Data.Maybe

-- Ex 1.1
safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]
safeLast :: [a] -> Maybe a
safeInit :: [a] -> Maybe [a]
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

safeInit [] = Nothing
safeInit [x] = Just []
safeInit (x:xs) = Just (x : fromMaybe [] (safeInit xs))

-- Ex 1.2
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs =
  case dropWhile f xs of
    [] -> []
    xs' -> x : splitWith f xs''
      where (x, xs'') = break f xs'

-- Ex 2.1,2
isNegative "" = False
isNegative s = head s == '-'

popNegative "" = ""
popNegative s
  | isNegative s = tail s
  | otherwise = s

type ErrorMessage = String

asInt :: String -> Either ErrorMessage Int
asInt "" = Left "bad"
asInt s =
  if not (all isDigit nonNegativeString)
    then Left "bad"
    else Right
           (negate *
            foldl
              (\acc digit -> acc * 10 + digitToInt digit)
              0
              nonNegativeString)
  where
    negate =
      if isNegative s
        then -1
        else 1
    nonNegativeString = popNegative s

-- Ex 2.3
concat' :: [[a]] -> [a]
concat' = foldr (++) []

-- Ex 2.4
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) =
  if p x
    then x : takeWhile' p xs
    else []

takeWhileFold :: (a -> Bool) -> [a] -> [a]
takeWhileFold p xs = fst (foldl (helper p) ([], True) xs)

helper :: (a -> Bool) -> ([a], Bool) -> a -> ([a], Bool)
helper pred (acc, notDone) v =
  if notDone && pred v
    then (v : acc, True)
    else (acc, False)

notMyTakeWhile :: (a -> Bool) -> [a] -> [a]
notMyTakeWhile p xs = foldr step [] xs
  where
    step x ys
      | p x = x : ys
      | otherwise = []

-- Ex 2.5
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy pred = foldr (groupByHelper pred) []

groupByHelper :: (a -> a -> Bool) -> a -> [[a]] -> [[a]]
groupByHelper pred v acc
  | null acc = [[v]]
  | null (head acc) = [v] : tail acc
  | pred v (head (head acc)) = (v : head acc) : tail acc
  | otherwise = [v] : acc

-- Ex 2.6
any' :: (a -> Bool) -> [a] -> Bool
any' pred = foldr (\x acc -> acc || pred x) False

cycle' :: [a] -> [a]
cycle' xs = foldr (\x acc -> xs ++ acc) [] [1 ..]

words' :: String -> [String]
words' =
  foldr
    (\c acc ->
       if isSpace c
         then [] : acc
         else (c : head acc) : tail acc)
    [[]]

unlines' :: [String] -> String
unlines' = foldr (\s acc -> acc ++ s ++ "\n") ""
