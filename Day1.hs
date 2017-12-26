solve :: (Num a, Eq a) => [a] -> a
solve = sum . fst . unzip . filter (\(x, y) -> x==y) . circPairs
    where
        circPairs :: [a] -> [(a, a)]
        circPairs l = zip l ((tail l) ++ [head l])


main :: IO ()
main = getLine >>= print . solve . (map (read . (:[])))
