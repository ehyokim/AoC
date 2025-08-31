module DaySix where
import Data.Map hiding (foldr, map, take, filter)
import Data.List hiding (lookup)
import Prelude hiding (lookup)
import GHC.Utils.Misc hiding (Direction)
import qualified Data.Set as Set
import Control.Monad (when)

data Direction = N | S | E | W
            deriving (Show, Eq)

type Coord = (Int, Int)
type Obstacles = [Coord]
type GuardPath = [Coord]
type ObstMap = (Map Int [Int], Map Int [Int])
type ObstDirMap = (Map Int [ProjCoordDir], Map Int [ProjCoordDir])

data GuardTrail = GuardTrail { getPath ::GuardPath, getDir :: Direction}
    deriving (Show)
data ProjCoordDir = PCD {pcdCoord :: Int, pcdDir :: Direction}
data Guard = Guard Direction Coord
    deriving (Show)
data Grid = Grid [String] (Int,Int)


main = readFile "./sample" >>= \file -> do let gridlst = lines file
                                           let grid = Grid gridlst ((length . head) gridlst - 1, length gridlst - 1)
                                           let parseRes = parseMapString grid
                                           --print $ conObstMap $ snd parseRes
                                           print $ findUniqueGuardSpots grid parseRes
                                           print $ findLoopObst grid parseRes


countLoopObst :: Grid -> (Guard, Obstacles) -> Int
countLoopObst grid guardObstPair = length $ findLoopObst grid guardObstPair  

findUniqueGuardSpots :: Grid -> (Guard, Obstacles) -> Int
findUniqueGuardSpots grid (initGuard, obstCoords) = length $ (Set.toList . Set.fromList) $ iterGuardMov grid obstMap initGuard
    where obstMap = conObstMap obstCoords

findLoopObst :: Grid -> (Guard, Obstacles) -> [[Coord]]
findLoopObst grid (initGuard, obstCoords) = calcAllLoopPoss grid guardTrailPairs
    where obstMap = conObstMap obstCoords
          guardTrailPairs = iterTrailMov grid obstMap (initGuard, (empty, empty))

iterGuardMov :: Grid -> ObstMap -> Guard -> GuardPath
iterGuardMov grid@(Grid _ (boundx, boundy)) obstMap guard
    | offmap               = guardTrail
    | otherwise            = guardTrail ++ iterGuardMov grid obstMap newGuard
    where guardStepRes = guardStep grid obstMap guard
          (newGuard, guardTrail, _, offmap) = guardStepRes

iterTrailMov :: Grid -> ObstMap -> (Guard, ObstDirMap) -> [(GuardTrail, ObstDirMap)]
iterTrailMov grid@(Grid _ (boundx, boundy)) obstMap (guard@(Guard dir _), hitObstMap) 
    | offmap               = [newTrailMapPair]
    | otherwise            = newTrailMapPair : iterTrailMov grid obstMap (newGuard, insertObstWithDir hitObstPos dir hitObstMap) 
    where guardStepRes = guardStep grid obstMap guard
          (newGuard, guardpath, hitObstPos, offmap) = guardStepRes

          guardTrail = GuardTrail guardpath dir
          newTrailMapPair = (guardTrail, hitObstMap)


concatLoopPoss :: Grid -> [(GuardTrail, ObstDirMap)] -> [Coord]
concatLoopPoss grid map = concat $ calcAllLoopPoss grid map

{- One edge case I am not considering, namely the case where the trail intersects with a previous path 
   in the direct opposite cardinal direction while having a possible loop obstruction. It cannot be a
   loop obstruction if this edge case is true.
-}
calcAllLoopPoss :: Grid -> [(GuardTrail, ObstDirMap)] -> [[Coord]]
calcAllLoopPoss grid = map (uncurry calcLoopPoss)
    where calcLoopPoss ::  GuardTrail -> ObstDirMap -> [Coord]
          calcLoopPoss trail obstMap = filter (checkLoopIfObstPlaced obstMap) (getPath trail)
            where rotGuardDir = (rotateRight . getDir) trail

                  checkLoopIfObstPlaced :: ObstDirMap -> Coord -> Bool
                  checkLoopIfObstPlaced obstMap coord = case lookup (constCoord coord) (constCoord obstMap) of
                                                                Nothing -> False
                                                                Just projCoordList -> findPrevHitObst (freeCoord coord) projCoordList
                  
                  findPrevHitObst :: Int -> [ProjCoordDir] -> Bool
                  findPrevHitObst l obstCoords
                        | alongES rotGuardDir = case find ((l <) . pcdCoord) obstCoords of
                                                    Nothing -> False 
                                                    Just z -> (pcdDir z == rotGuardDir)
                        | otherwise           = case find ((l >) . pcdCoord) (reverse obstCoords) of                                                   
                                                    Nothing -> False
                                                    Just z -> (pcdDir z == rotGuardDir)

                  constCoord :: (a,a) -> a 
                  constCoord = if alongX rotGuardDir then snd else fst

                  freeCoord :: (a,a) -> a
                  freeCoord = if alongX rotGuardDir then fst else snd


guardStep :: Grid -> ObstMap -> Guard -> (Guard, GuardPath, Coord, Bool)
guardStep grid obstMap guard@(Guard dir _) = (Guard newGuardDir newGuardPos, 
                                              guardPath, 
                                              hitObstPos, 
                                              isOffMap)
    where guardPath = retGuardTrail grid obstMap guard
          newGuardPos = findNewGuardPos dir guardPath
          newGuardDir = rotateRight dir
          hitObstPos = hitObstCoord dir newGuardPos
          isOffMap = deterGuardOffMap grid dir newGuardPos

          findNewGuardPos :: Direction -> ([Coord] -> Coord)
          findNewGuardPos dir
            | alongES dir = last
            | otherwise   = head


{- Given a direction and a coordinate of a guard, this function determines the coordinate of
   obstacle that the guard hit.  
 -}
hitObstCoord :: Direction -> Coord -> Coord
hitObstCoord dir (x,y)
  | dir == E  = (x+1,y)
  | dir == W  = (x-1,y)
  | dir == S  = (x,y+1)
  | otherwise = (x,y-1)

{- Given a direction and a coordinate of a guard, determine the guard has walked off the map.
 -}
deterGuardOffMap :: Grid -> Direction -> Coord -> Bool
deterGuardOffMap (Grid _ (boundx, boundy)) guardDir newGuardPos = let (newGuardX, newGuardY) = newGuardPos in
                                                                ((guardDir == W) && (newGuardX == 0)) ||
                                                                ((guardDir == E) && (newGuardX == boundx)) ||
                                                                ((guardDir == N) && (newGuardY == 0)) ||
                                                                ((guardDir == S) && (newGuardY == boundy))

retGuardTrail :: Grid -> ObstMap -> Guard -> GuardPath
retGuardTrail (Grid _ (boundx, boundy)) (xObstMap, yObstMap) (Guard dir (x,y))
    | alongX dir = case lookup y yObstMap of
                    Nothing -> map (,y) $ fullRange dir
                    Just coordlst -> map (,y) $ findGuardRange x coordlst dir
    | otherwise  = case lookup x xObstMap of
                    Nothing -> map (x,) $ fullRange dir
                    Just coordlst -> map (x,) $ findGuardRange y coordlst dir       
    where fullRange :: Direction -> [Int]
          fullRange dir
            | dir == W  = [0..x]
            | dir == E  = [x..boundx]
            | dir == N  = [0..y]
            | otherwise = [y..boundy]
          
          findGuardRange :: Int -> [Int] -> Direction -> [Int]
          findGuardRange l obstCoords dir
            | alongES dir = case find (l <) obstCoords of
                                Nothing -> fullRange dir
                                Just z -> [l..(z-1)] 
            | otherwise   = case find (l >) (reverse obstCoords) of                                                   
                                Nothing -> fullRange dir
                                Just z -> [(z+1)..l]

alongX :: Direction -> Bool 
alongX dir = (dir == E) || (dir == W)

alongY :: Direction -> Bool
alongY dir = (dir == N) || (dir == S)

alongES :: Direction -> Bool
alongES dir = (dir == E) || (dir == S)

rotateRight :: Direction -> Direction
rotateRight N = E
rotateRight E = S
rotateRight S = W
rotateRight W = N

conObstMap :: Obstacles -> ObstMap
conObstMap = foldr insertObst (empty,empty)

insertObst :: Coord -> ObstMap -> ObstMap
insertObst (x,y) (xmap, ymap) = (insertWith insertSortedPl x [y] xmap, 
                                 insertWith insertSortedPl y [x] ymap)
    where insertSortedPl :: [Int] -> [Int] -> [Int]
          insertSortedPl val@[w] (z:zs)
            | w > z = z : insertSortedPl val zs
            | otherwise = w:z:zs

insertObstWithDir :: Coord -> Direction -> ObstDirMap -> ObstDirMap
insertObstWithDir (x,y) dir (xmap, ymap) = (insertWith insertSortedPl x [PCD y dir] xmap, 
                                            insertWith insertSortedPl y [PCD x dir] ymap)
    where insertSortedPl :: [ProjCoordDir] -> [ProjCoordDir] -> [ProjCoordDir]
          insertSortedPl new [] = new
          insertSortedPl val@[w] (z:zs)
            | wc > zc = z : insertSortedPl val zs
            | otherwise = w:z:zs
            where PCD wc _ = w
                  PCD zc _ = z

parseMapString :: Grid -> (Guard, Obstacles)
parseMapString grid = (findGuard grid, findObst grid) 
    where findObst :: Grid -> Obstacles  
          findObst (Grid grid (boundx, boundy)) = [(x,y) | x <- [0..boundx], 
                                                           y <- [0..boundy], 
                                                           let char = (grid !! y) !! x, 
                                                           char /= '.' && char == '#' ]

          findGuard :: Grid -> Guard
          findGuard (Grid grid (boundx, boundy)) = head [ Guard N (x,y) | x <- [0..boundx], 
                                                                          y <- [0..boundy], 
                                                                          let char = (grid !! y) !! x, 
                                                                          char /= '.' && char == '^' ]


