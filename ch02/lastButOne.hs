lastButOne :: [a] -> a

lastButOne xs = last (take  (length xs - 1) xs)
