palindrome [] = []
palindrome (x:xs) = (x : palindrome xs) ++ [x]

isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs)
  | x == (last xs) = isPalindrome (init xs)
  | otherwise      = False
