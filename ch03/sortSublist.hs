import Data.List

lengthCompare xs ys = compare (length xs) (length ys)

sortSublist xss = sortBy lengthCompare xss
