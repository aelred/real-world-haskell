myLength :: [a] -> Integer
myLength (x:xs) = 1 + myLength xs
myLength []   = 0
