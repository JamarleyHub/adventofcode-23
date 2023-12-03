{-
        Find index of everything that is NOT a . or a number and save it in a list
        Find indices of every number (-1 and +1) and save them in a list 
        Zip lists: 
                once normally, once with a shifted up one row, once with a shifted down a row 
                if the numberindice ranges overlap with symbols, we have a hit, we save that result.
        Sum up result list
         
-}

import Data.Char (isDigit)
import Data.List (elemIndices, nub, transpose)

symbolList :: [String] -> [[Int]]
symbolList = map (nub . concatMap (\x -> [x - 1, x, x + 1]) . elemIndices 'X' . map findSymbol)

findSymbol :: Char -> Char 
findSymbol x
        | '.' == x = x
        | isDigit x = x
        | otherwise = 'X'

digitList :: String -> [Int]
digitList str = [x | (char, x) <- zip str [0..], isDigit char]

overlap :: [Int] -> [Int] -> [Int]
overlap xs [] = xs
overlap [] xs = xs
overlap (x:xs) (y:ys) 
        | x > y = y : overlap (x:xs) ys
        | x < y = x : overlap xs (y:ys)
        | otherwise = x : overlap xs ys

main :: IO ()
main = do
        content <- readFile "inputtest"
        print content
        let symbols = symbolList (lines content)
        let shiftBack = [] : init symbols 
        let shiftForward = tail symbols ++ [[]]
        print (zipWith overlap (zipWith overlap shiftForward symbols) shiftBack)
        print symbols
        print (map digitList (lines content))
