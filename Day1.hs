solve :: (Num a, Eq a) => Int -> [a] -> a
solve gap xs = sum $ zipWith (\x y -> if x==y then x else 0) xs (drop gap xs ++ take gap xs)

main :: IO ()
main = do
    xs <- fmap (map (read . (:[]))) getLine
    
    --part one:
    print $ solve 1 xs
    
    -- part two:
    print $ solve ((length xs) `quot` 2) xs
