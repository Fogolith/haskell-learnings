elementAt :: [a] -> Int -> Maybe a
elementAt [] _ = Nothing
elementAt x 0 = Nothing
elementAt x y = Just (x!!(y-1))
