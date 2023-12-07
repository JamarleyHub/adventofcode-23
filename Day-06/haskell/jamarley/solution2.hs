import Data.List.Split (splitOn)
import Data.Char (isSpace)

sanitize :: String -> Int 
sanitize = read . filter (not . isSpace) . drop 1 . dropWhile (/= ':')

possibleRaceConfigs :: Int -> [Int]
possibleRaceConfigs x = zipWith (*) [0..x] [x,(x-1)..0]

betterThanRecord :: [Int] -> Int -> [Int]
betterThanRecord xs y = filter (> y) xs 

main :: IO ()
main = do
        content <- readFile "input"
        let     sanitizedContent = map sanitize (lines content)
                tupleList = (\[x, y] -> (x,y)) sanitizedContent
                raceDurations = (possibleRaceConfigs . fst) tupleList
                records = snd tupleList
        print (length (betterThanRecord raceDurations records))
