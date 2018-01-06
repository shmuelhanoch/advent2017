checksum :: ([Int] -> Int) ->[[Int]] -> Int
checksum f sheet = sum $ map f sheet

maxDiff :: [Int] -> Int
maxDiff l = maximum l - minimum l

divided :: [Int] -> Bool
divided [n, k] = n `mod` k == 0 && k /= 1 && n /= k

dividedDiff :: [Int] -> Int
dividedDiff l = n `div` k
    where
        [n, k] = head $ filter divided $ sequence [l, l]

main :: IO ()
main = do
    sheet <- map (map read . words) . lines <$> getContents
    
    --part one
    print $ checksum maxDiff sheet
    
    --part two
    print $ checksum  dividedDiff sheet 

