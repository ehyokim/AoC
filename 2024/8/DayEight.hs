module DayEight where
import qualified Data.Map as Map 
import qualified Data.List as List
import Data.Char (isAlphaNum)

data Coord = Coord {xCoord :: Int, yCoord :: Int}
    deriving (Show, Eq)
type Bounds = (Int, Int)
type TowerFreqMap = Map.Map Char [Coord]
data TowerPair = TP Coord Coord
data Grid = Grid [String] Bounds
data Distance = Dis {xDis :: Int, yDis :: Int}

main = readFile "./input" >>= \file -> do  let gridlst = lines file
                                           let grid = Grid gridlst ((length .head) gridlst - 1, length gridlst - 1)
                                           let freqMap = constFreqMap grid
                                           print $ countAntiNodes grid freqMap


countAntiNodes :: Grid -> TowerFreqMap -> Int
countAntiNodes (Grid _ bounds) freqMap = length $ List.nub totalAN
    where totalAN = Map.foldr (\x s -> (++ s) $ concatMap (getAntiNodes bounds) $ genComb x) [] freqMap

constFreqMap :: Grid -> TowerFreqMap
constFreqMap (Grid gridlst (boundx, boundy)) = Map.fromListWith sortFunc towerFreqPairs
    where sortFunc :: [Coord] -> [Coord] -> [Coord]
          sortFunc [c] [] = [c]
          sortFunc new@[c] (z:zs)
            | zy < cy = z : sortFunc new zs
            | otherwise = c : z : zs
            where Coord cx cy = c
                  Coord zx zy = z
        
          towerFreqPairs = [(char, [Coord x y]) | x <- [0..boundx], 
                                                  y <- [0..boundy],
                                                  let char = (gridlst !! y) !! x, 
                                                  isAlphaNum char]
                                

data Direction = UP | DOWN

-- For this to work, it's crucial that the first tower is above or at the same y-value as the second tower. 
getAntiNodes :: Bounds -> TowerPair -> [Coord]
getAntiNodes bounds (TP tower1 tower2) = (findCollinNodes (Coord (xCoord tower1) (yCoord tower1)) UP) ++ 
                                         (findCollinNodes (Coord (xCoord tower2) (yCoord tower2)) DOWN)
    where pairdis = getDistance tower1 tower2
          xdis = xDis pairdis
          ydis = yDis pairdis

          findCollinNodes :: Coord -> Direction -> [Coord]
          findCollinNodes coord dir
            | checkBounds bounds coord = case dir of 
                                            UP -> coord : findCollinNodes (Coord (xCoord coord + xdis) (yCoord coord + ydis)) UP
                                            DOWN -> coord : findCollinNodes (Coord (xCoord coord - xdis) (yCoord coord - ydis)) DOWN
            | otherwise                = []

checkBounds :: Bounds -> Coord -> Bool
checkBounds (xbound, ybound) (Coord x y)
    | x < 0 || x > xbound = False
    | y < 0 || y > ybound = False
    | otherwise           = True

getDistance :: Coord -> Coord -> Distance
getDistance (Coord x1 y1) (Coord x2 y2) = Dis {xDis = x1 - x2, yDis = y1 - y2}

genComb :: [Coord] -> [TowerPair]
genComb [x] = []
genComb (x:xs) = map (TP x) xs ++ genComb xs  
genComb _ = error "Coordinate list cannot be empty."