import Data.List.Split (splitOn)

sanitize :: String -> [Int]
sanitize = map read . filter (/= "") . splitOn " " . drop 1 . dropWhile (/= ':')

possibleRaceConfigs :: Int -> [Int]
possibleRaceConfigs x = zipWith (*) [0..x] [x,(x-1)..0]

betterThanRecord :: [[Int]] -> [Int] -> [[Int]]
betterThanRecord (x:xs) (y:ys) = filter (> y) x : betterThanRecord xs ys
betterThanRecord _ _ = []

main :: IO ()
main = do
        content <- readFile "input"
        let     sanitizedContent = map sanitize (lines content)
                tupleList = (\[xs, ys] -> zip xs ys) sanitizedContent
                raceDurations = map (possibleRaceConfigs . fst) tupleList
                records = map snd tupleList
        print (product $ map length (betterThanRecord raceDurations records))
