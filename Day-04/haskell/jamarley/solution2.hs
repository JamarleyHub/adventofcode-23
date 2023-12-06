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
extend (x:xs) (y:ys) = y : extend xs (zipWithRest (map (+y) (take x ys)) ys)

zipWithRest :: [Int] -> [Int] -> [Int]
zipWithRest _ [] = []
zipWithRest [] ys = ys
zipWithRest (x:xs) (y:ys) = x : zipWithRest xs ys

main :: IO ()
main = do
        content <- readFile "input"
        let input = lines content
            sanitizedInput = map sanitize input
            listCount = take (length $ map dropSecond sanitizedInput) [1,1..] :: [Int]
        print (countDuplicates sanitizedInput)
        print (sum $ extend (countDuplicates sanitizedInput) listCount)
