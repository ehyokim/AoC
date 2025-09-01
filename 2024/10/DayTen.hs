module DayTen where
-- Do some more research on Data.Vector and Data.Sequence.
import Data.Char (digitToInt)
import Data.Set hiding (foldr, map, filter)

type Coord = (Int, Int)
type Score = Int

data TopoCell = TC { tcCoord :: Coord,  tcPeakIdent :: Set Int}
data TopoScore = TS { tsCoord :: Coord, tsScore :: Int } 
data Grid = Grid [[Int]] (Int, Int)

main = readFile "./input" >>= \file -> do  let gridlst = (map (map digitToInt) . lines) file
                                           let grid = Grid gridlst ((length . head) gridlst - 1, length gridlst - 1)
                                           print $ calcReachSet grid 9 []
                                           print $ calcRank grid 9 []

calcReachSet :: Grid -> Int -> [TopoCell] -> Int
calcReachSet grid@(Grid _ (boundx, boundy)) topoval topoCellsLst 
    | topoval == 0 = foldr ((+) . size . tcPeakIdent) 0 nextTopoCellsLst
    | topoval < 9 = calcReachSet grid (topoval - 1) nextTopoCellsLst
    | topoval == 9 = let numOfPeaks = length newTopoValCells
                         initPeakIdentSets = map singleton [1..numOfPeaks]
                     in calcReachSet grid (topoval - 1) (zipWith TC newTopoValCells initPeakIdentSets)
    | otherwise = error "topoval should not be greater than 9"
    where newTopoValCells = cellsWithTopoVal grid topoval
          nextTopoCellsLst = map (constTopoCell topoCellsLst) newTopoValCells

calcRank :: Grid -> Int -> [TopoScore] -> Int
calcRank grid@(Grid _ (boundx, boundy)) topoval topoCellsLst 
    | topoval == 0 = foldr ((+) . tsScore) 0 nextTopoScoreLst
    | topoval < 9 = calcRank grid (topoval - 1) nextTopoScoreLst
    | topoval == 9 = calcRank grid (topoval - 1) (map (`TS` 1) newTopoValCells)
    | otherwise = error "topoval should not be greater than 9"
    where newTopoValCells = cellsWithTopoVal grid topoval
          nextTopoScoreLst = map (constTopoScore topoCellsLst) newTopoValCells


constTopoCell :: [TopoCell] -> Coord -> TopoCell
constTopoCell topoCellsLst coord = TC { tcCoord = coord , tcPeakIdent = cellSet}
  where cellSet = mergeTopoCellSets $ filter (isNeighbor coord . tcCoord) topoCellsLst

mergeTopoCellSets :: [TopoCell] -> Set Int
mergeTopoCellSets = foldr (union . tcPeakIdent) empty

constTopoScore :: [TopoScore] -> Coord -> TopoScore
constTopoScore topoCellsLst coord = TS { tsCoord = coord , tsScore = cellScore}
  where cellScore = sumTopoScores $ filter (isNeighbor coord . tsCoord) topoCellsLst

sumTopoScores :: [TopoScore] -> Int
sumTopoScores = foldr ((+) . tsScore) 0

cellsWithTopoVal :: Grid -> Int -> [Coord]
cellsWithTopoVal (Grid gridlst (boundx, boundy)) topoval = [coord | x <- [0..boundx], y <- [0..boundy],
                                                              let coord = (x,y), 
                                                              let cellval = getCharFromGrid gridlst coord,
                                                              cellval == topoval]

isNeighbor :: Coord -> Coord -> Bool
isNeighbor (x1, y1) (x2, y2)
    | (y1 == y2) && ((x1 - 1) == x2 || (x1 + 1) == x2) = True
    | (x1 == x2) && ((y1 - 1) == y2 || (y1 + 1) == y2) = True
    | otherwise                                        = False 

getCharFromGrid :: [[Int]] -> Coord -> Int
getCharFromGrid gl (x,y) = (gl !! y) !! x