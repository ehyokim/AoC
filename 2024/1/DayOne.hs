module DayOne where
import Data.List
import GHC.Utils.Misc

main = partOne >> partTwo

partOne = readFile "./input" >>= \file ->
                                    do let listPairs = map words . lines $ file
                                       let (leftList, rightList) = applyPair sort $ unzip $ map readlistToPair listPairs
                                       print $ sum $ zipWith (\n -> abs . (n-)) leftList rightList

partTwo = readFile "./input" >>= \file ->
                                    do let listPairs = map words . lines $ file
                                       let (leftList, rightList) = unzip $ map readlistToPair listPairs
                                       print $ sum $ map (\x -> ((*x) . count (== x)) rightList) leftList


readlistToPair :: [String] -> (Int, Int)
readlistToPair [x,y] = (read x :: Int, read y :: Int)
readlistToPair _ = error "Malformed List"

applyPair :: (a -> b) -> (a,a) -> (b,b)
applyPair f (x, y) = (f x, f y)        
                
