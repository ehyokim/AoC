module DayEleven where
import Data.Char (digitToInt)

-- Research more about memoization.

main = readFile "./input" >>= \file -> do let arg = (map (\x -> read x :: Int) . words) file
                                          print $ pOneNumStones arg


pOneNumStones :: [Int] -> Int
pOneNumStones = length . last . take 76 . iterate blink

blink :: [Int] -> [Int]
blink [] = []
blink (x:xs)
    | x == 0 = 1 : blink xs
    | evenDigits = let (x,y) = divideDigits
                   in x : y : blink xs
    | otherwise = (2024 * x) : blink xs
    where numStr = show x
          numDigits = length numStr
          evenDigits = even numDigits

          divideDigits :: (Int, Int)
          divideDigits = (read (take half numStr) :: Int, 
                          read (drop half numStr) :: Int)
            where half = numDigits `div` 2