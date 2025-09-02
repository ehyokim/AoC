module DayTwelve where
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Map as Map
import GHC.Utils.Misc hiding (singleton)
import Data.Set (Set, singleton, empty, unions, size, member, insert)
import qualified Data.Set as Set
import qualified Data.List as List

type PlotType = Char
type Coord = (Int, Int)
type Bounds = (Int, Int)
type Area = Int 
type Peri = Int 

data GardenRegion = GR { grType :: PlotType, grPlots :: [Set Coord]}
    deriving (Show)
data GardenStat = GS {gsType :: PlotType, gsStats :: [(Area, Peri)]}
    deriving (Show)
data Grid = Grid {getGrid :: V.Vector (U.Vector Char), getBounds :: Bounds }

main = readFile "./input" >>= \file -> do  let gridVec = constGridVec $ lines file
                                           let grid = Grid { getGrid = gridVec, getBounds = ((U.length . V.head) gridVec - 1, V.length gridVec - 1) }
                                           let grlst = constRegionLst grid
                                           --print $ getAllPairs grid grlst
                                           print $ calcTotalPrice grid grlst


calcTotalPrice :: Grid -> [GardenRegion] -> Int
calcTotalPrice grid = List.foldr ((+) . uncurry (*)) 0 . concatMap gsStats . getAllPairs grid

getAllPairs :: Grid -> [GardenRegion] -> [GardenStat]
getAllPairs grid = map (getRegAreaPeriPair grid)

getRegAreaPeriPair :: Grid -> GardenRegion -> GardenStat
getRegAreaPeriPair grid gr = GS { gsType = grType gr, gsStats = zip (calcAreaRegion gr) (calcPeriRegion grid gr) }

calcPeriRegion :: Grid -> GardenRegion -> [Int] 
calcPeriRegion grid gr = map (\p -> Set.foldl' (updatePeri p) 0 p) plots
    where updatePeri :: Set Coord -> Int -> Coord -> Int
          updatePeri plot accum coord = (4 - count (`member` plot) neighbors) + accum
            where neighbors = getNeighbors grid coord

          plots = grPlots gr

calcSidesRegion :: Grid -> GardenRegion -> [Int]
calcSidesRegion grid gr = 
    where updateSides :: Set Coord -> Int -> Coord -> Int
          updateSides 

getNeighbors :: Grid -> Coord -> [Coord]
getNeighbors grid (x,y) = filter (checkBounds grid) [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

checkBounds :: Grid -> Coord -> Bool
checkBounds (Grid _ (boundx, boundy)) (x,y) = (x >= 0 && x <= boundx) &&
                                              (y >= 0 && y <= boundy)

calcAreaRegion :: GardenRegion -> [Int]
calcAreaRegion = map size . grPlots

constRegionLst :: Grid -> [GardenRegion]
constRegionLst grid@(Grid _ (boundx, boundy)) = List.foldl' buildRegLst [] plotCoords
    where 
          buildRegLst ::  [GardenRegion] -> (PlotType, Coord) -> [GardenRegion]
          buildRegLst [] (pt, coord) = [GR pt [singleton coord]]
          buildRegLst (r:rs) arg@(pt, coord)
            | rpt == pt = GR rpt (mergeRegSets coord rset) : rs
            | otherwise = r : buildRegLst rs arg
            where GR rpt rset = r
          
          mergeRegSets ::  Coord -> [Set Coord] -> [Set Coord]
          mergeRegSets coord regsets = let (inRegs, outRegs) = List.partition (isPartofRegion grid coord) regsets
                                       in (coord `insert` unions inRegs)  : outRegs  

          isPartofRegion :: Grid -> Coord -> Set Coord -> Bool
          isPartofRegion grid coord regset = any (`elem` regset) (getNeighbors grid coord)
         
          plotCoords = [ (pt, coord) | x <- [0..boundx], y <- [0..boundy],
                                                let coord = (x,y),
                                                let pt = getPlotType grid coord ] 

constGridVec :: [String] -> V.Vector (U.Vector Char)
constGridVec gridlst = V.fromList [ U.fromList row | row <- gridlst ]

getPlotType :: Grid -> Coord -> Char
getPlotType grid (x,y) = (getGrid grid V.! y) U.! x
