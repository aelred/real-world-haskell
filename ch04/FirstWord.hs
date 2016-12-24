import InteractWith
import Data.Maybe (fromMaybe)

main = interactMain firstWordOfEachLine

firstWordOfEachLine :: String -> String
firstWordOfEachLine = unlines . map (fromMaybe "" . firstWord) . lines

firstWord :: String -> Maybe String
firstWord = safeHead . words

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead []     = Nothing
