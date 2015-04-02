import Data.Char (digitToInt, isSpace)

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
asInt_fold [] = error "Empty string"
asInt_fold (x:xs)
  | x == '-'  = - (asInt_fold xs)
  | otherwise = foldl step 0 (x:xs)
    where step acc x = acc * 10 + toInteger (digitToInt x)

type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Integer
asInt_either [] = Left "Empty string"
asInt_either (x:xs)
  | x == '-'  = case asInt_either xs of
                    Right num -> Right (-num)
                    Left err -> Left err
  | otherwise = Right (foldl step 0 (x:xs))
    where step acc x = acc * 10 + toInteger (digitToInt x)

concat :: [[a]] -> [a]
concat xss = foldr (++) [] xss

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
