{-
        [1,1,1,1,1,1]
        [1,2,2,2,2,1]
        [1,2,4,4,2,1]

        add always one step forward:
        list = take x . listCount
        rec x xs
                | x > 0 = rec (x - 1) (map (+) xs list) 
                | otherwise = (map (+) xs list)
        rec _ _ = []

        zipWith (+) orig list
-}


import Data.List.Split (splitOn)

sanitize :: String -> [[Int]]
sanitize = map (map read . words) . splitOn "|" . drop 2 . dropWhile (/= ':')

dropSecond :: [[Int]] -> [Int]
dropSecond (_:[x]) = x
dropSecond _ = []

countDuplicates :: [[[Int]]] -> [Int]
countDuplicates ([x,y]:xs) = length (filter (`elem` y) x) : countDuplicates xs
countDuplicates _ = []

extend :: [Int] -> [Int] -> [Int]
extend _ [] = []
extend [] _ = []
extend (x:xs) (y:ys)
        | head ys > 1 = y : extend xs (zipWithRest (^) (take x ys) ys)
        | otherwise = y : extend xs (zipWithRest (+) (take x ys) ys)

zipWithRest :: (Int -> Int -> Int) -> [Int] -> [Int] -> [Int]
zipWithRest _ [] xs = xs
zipWithRest _ xs [] = xs 
zipWithRest p (x:xs) (y:ys) = p x y : zipWithRest p xs ys 

main :: IO ()
main = do
        content <- readFile "inputtest"
        let input = lines content
            sanitizedInput = map sanitize input
            listCount = take (length $ map dropSecond sanitizedInput) [1,1..] :: [Int]
        print (countDuplicates sanitizedInput)
        print (extend (countDuplicates sanitizedInput) listCount)
