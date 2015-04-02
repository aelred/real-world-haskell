import InteractWith

main = interactMain (
    unlines .
    (map head) .
    (filter (not . null)) .
    (map words) .
    lines)
