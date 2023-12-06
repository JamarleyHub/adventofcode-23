import Data.List.Split (splitOn)

sanitize :: String -> [[Integer]]
sanitize = map (map read . words) . splitOn "|" . drop 2 . dropWhile (/= ':')

countDuplicates :: [[[Integer]]] -> [Int]
countDuplicates ([x,y]:xs) = length (filter (`elem` y) x) : countDuplicates xs
countDuplicates _ = []

main :: IO ()
main = do
        content <- readFile "input"
        let input = lines content
            sanitizedInput = map sanitize input
        print (sum . map (\x -> 2^(x - 1)) . filter (/= 1) $ countDuplicates sanitizedInput)
