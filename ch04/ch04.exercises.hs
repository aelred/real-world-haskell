import Data.Char (digitToInt, isDigit, isSpace)

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead []     = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x:xs) = Just xs
safeTail []     = Nothing

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs) 
safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs = ys : splitWith p (dropWhile (not . p) zs)
  where (ys, zs) = span p xs

asInt_fold :: String -> Integer
asInt_fold ""       = 0
asInt_fold ('-':xs) = - (asInt_fold xs)
asInt_fold xs       = foldl step 0 xs
    where step acc x = acc * 10 + toInteger (digitToInt x)

type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Integer
asInt_either []       = Left "Empty string"
asInt_either ('-':xs) = fmap negate $ asInt_either xs
asInt_either xs       = foldl step (Right 0) xs
    where step (Left  err) _ = Left err
          step (Right acc) x
            | isDigit x = Right $ acc * 10 + toInteger (digitToInt x)
            | otherwise = Left $ "non-digit '" ++ [x] ++ "'"

concat :: [[a]] -> [a]
concat = foldr (++) []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x       = x : (takeWhile' p xs)
  | otherwise = []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' p = foldr step []
    where step x acc
            | p x       = x : acc
            | otherwise = []

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' = undefined

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr (\a b -> p a || b) False

cycle' :: [a] -> [a]
cycle' xs = foldr (:) (cycle' xs) xs

words' :: String -> [String]
words' xs = foldr step [[]] xs
    where step x (xs:xss)
            | not (isSpace x) = (x:xs):xss
            | null xs         = xs:xss
            | otherwise       = []:xs:xss

unlines' :: [String] -> String
unlines' xss = foldr step "" xss
    where step xs acc = xs ++ '\n':acc
