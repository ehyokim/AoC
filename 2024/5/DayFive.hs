module DayFive where
import Data.Map hiding (foldr, map)
import Data.List.Split
import Data.List hiding (lookup)
import Data.Ord
import Prelude hiding (lookup)

type OrderedRel = (Int, Int)
type Update = [Int]
type OrderMap = Map Int [Int]

main = readFile "./input" >>= \file -> do  let [orderList, updateList] = splitOn [""] $ lines file
                                           let parsedUpdateList = map (map (\x -> read x :: Int) . splitOn ",") updateList
                                           let orderMap = parseOrderList orderList
                                           print $ calcScore orderMap parsedUpdateList
                                           print $ calcFixedScore orderMap parsedUpdateList 




calcFixedScore :: OrderMap -> [Update] -> Int 
calcFixedScore orderMap = foldr ((+) . calcFixedUpdateScore orderMap) 0 
        where calcFixedUpdateScore :: OrderMap -> Update -> Int
              calcFixedUpdateScore omap update
                | checkUpdate omap update = 0
                | otherwise               = fixUpdate omap update !! (length update `div` 2) 

fixUpdate :: OrderMap -> Update -> Update
fixUpdate omap = sortBy (ordering omap)

ordering :: OrderMap -> Int -> Int -> Ordering
ordering omap x y = case lookup x omap of
                        Nothing -> GT
                        Just ltlst -> if y `elem` ltlst then LT else GT

calcScore ::  OrderMap -> [Update] -> Int
calcScore orderMap = foldr ((+) . calcUpdateScore orderMap) 0 
        where calcUpdateScore :: OrderMap -> Update -> Int
              calcUpdateScore omap update
                | checkUpdate omap update = update !! (length update `div` 2)
                | otherwise              = 0

parseOrderList :: [String] -> OrderMap
parseOrderList = foldr (insertMap . parseRule) empty
    where parseRule :: String -> (Int, Int)
          parseRule = toPair . map (\x -> read x :: Int) . splitOn "|" 
         
          insertMap :: (Int,Int) -> OrderMap -> OrderMap
          insertMap (k,v) = insertWith (++) k [v]

          toPair :: [a] -> (a,a)
          toPair [x,y] = (x,y)
          toPair _ = error "Malformed List" 

checkUpdate :: OrderMap -> Update -> Bool
checkUpdate _ [x] = True
checkUpdate orderMap (x:xs) = case lookup x orderMap of
                                Nothing -> all (not . searchElem orderMap x) xs && checkUpdate orderMap xs
                                Just gtlst -> all (`elem` gtlst) xs && checkUpdate orderMap xs
    where searchElem :: OrderMap -> Int -> Int -> Bool
          searchElem omap xc x = case lookup x omap of
                                    Nothing -> error "Malformed Order List"
                                    Just ltlst -> xc `elem` ltlst