module DayTwo where
import GHC.Read (list)
import GHC.Utils.Misc

main = checkTotalSafety checkSafety >> checkTotalSafety checkSafetyDamp

checkTotalSafety :: ([Int] -> Bool) -> IO ()
checkTotalSafety safetyF = readFile "./input" >>= \file -> do 
                                          let fileList = map words $ lines file
                                          let safeBoolList = map (safetyF . map (\x -> read x :: Int)) fileList
                                          print $ count id safeBoolList 

checkSafety :: [Int] -> Bool
checkSafety list = checkOrder list && checkGap list
    where checkOrder l = (and $ checkElem l (<)) || (and $ checkElem l (>))
          checkGap l = and $ checkElem l (\n -> (<= 3) . abs . (n-))

checkElem :: [Int] -> (Int -> Int -> Bool) -> [Bool]
checkElem list f
    | length list < 2 = []
    | otherwise = let (x:y:xs) = list in 
                      f x y : checkElem (y:xs) f

-- Part two. Easiest solution is to just rescan the list after removing a candidate. 

checkSafetyDamp :: [Int] -> Bool
checkSafetyDamp list = checkInc list || checkDec list || checkInc rlist || checkDec rlist
    where checkInc l = and $ checkElemDamp l incPred False 
          checkDec l = and $ checkElemDamp l decPred False
          incPred x y = (x < y) && ((<= 3) $ abs $ x - y)
          decPred x y = (x > y) && ((<= 3) $ abs $ x - y)
          rlist = reverse list  

checkElemDamp :: [Int] -> (Int -> Int -> Bool) -> Bool -> [Bool]
checkElemDamp list f tryflag
    | length list < 2 = []
    | otherwise = let (x:y:xs) = list
                      sat = f x y 
                  in 
                      if not sat && not tryflag then 
                        checkElemDamp (x:xs) f True
                      else 
                        sat : checkElemDamp (y:xs) f tryflag 
                        