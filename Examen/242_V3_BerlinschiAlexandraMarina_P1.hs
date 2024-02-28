cuMonade :: [Int] -> [Int]
cuMonade = (=<<) f
  where
    f x | x `elem` [11, 35, 55] = return (x^2)
        | even x && x > 10 = []
        | otherwise = return x >> return x
faraMonade :: [Int] -> [Int]
faraMonade = concatMap f
    where
        f x | x `elem` [11, 35, 55] = [x^2]
            | even x && x > 10 = []
            | otherwise = [x, x]
