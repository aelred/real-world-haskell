intersperse :: a -> [[a]] -> [a]
intersperse _ [x] = x
intersperse y (x:xs) = x ++ (y : intersperse y xs)
intersperse _ [] = []
