module DaySix where
import Data.Map hiding (foldr, map, take)
import Data.List hiding (lookup)
import Prelude hiding (lookup)
import GHC.Utils.Misc hiding (Direction)
import qualified Data.Set as Set

data Direction = N | S | E | W
            deriving (Show, Eq)

type Coord = (Int, Int)
type Obstacles = [Coord]
type GuardPath = [Coord]
type ObstMap = (Map Int [Int], Map Int [Int])

data GuardTrail = GuardTrail { getPath ::GuardPath, getDir :: Direction}
    deriving (Show)
data Guard = Guard Direction Coord
    deriving (Show)
data Grid = Grid [String] (Int,Int)


main = readFile "./small" >>= \file -> do  let gridlst = lines file
                                           let grid = Grid gridlst ((length . head) gridlst - 1, length gridlst - 1)
                                           let parseRes = parseMapString grid
                                           --print $ conObstMap $ snd parseRes
                                           print $ findUniqueGuardSpots grid parseRes
                                           print $ findLoopObstCount grid parseRes

findUniqueGuardSpots :: Grid -> (Guard, Obstacles) -> Int
findUniqueGuardSpots grid (initGuard, obstCoords) = length $ (Set.toList . Set.fromList) $ iterGuardMov grid obstMap initGuard
    where obstMap = conObstMap obstCoords

findLoopObstCount :: Grid -> (Guard, Obstacles) -> Int
findLoopObstCount grid (initGuard, obstCoords) = sum $ countLoopObst $ iterTrailMov grid obstMap initGuard
    where obstMap = conObstMap obstCoords

iterGuardMov :: Grid -> ObstMap -> Guard -> GuardPath
iterGuardMov grid@(Grid _ (boundx, boundy)) obstMap guard
    | offmap               = guardTrail
    | otherwise            = guardTrail ++ iterGuardMov grid obstMap newGuard
    where guardStepRes = guardStep grid obstMap guard
          (newGuard, guardTrail, offmap) = guardStepRes

iterTrailMov :: Grid -> ObstMap -> Guard -> [GuardTrail]
iterTrailMov grid@(Grid _ (boundx, boundy)) obstMap guard@(Guard dir _)
    | offmap               = [guardTrail]
    | otherwise            = guardTrail : iterTrailMov grid obstMap newGuard
    where guardStepRes = guardStep grid obstMap guard
          (newGuard, guardpath, offmap) = guardStepRes
          guardTrail = GuardTrail guardpath dir

{- One edge case I am not considering, namely the case where the trail intersects with a previous path 
   in the direct opposite cardinal direction while having a possible loop obstruction. It cannot be a
   loop obstruction if this edge case is true.
-}
countLoopObst :: [GuardTrail] -> [Int]
countLoopObst = findLoopObst . reverse
    where findLoopObst :: [GuardTrail] -> [Int]
          findLoopObst [] = [0]
          findLoopObst (x:xs) = count (isLoopPoss x) xs : findLoopObst xs

          isLoopPoss :: GuardTrail -> GuardTrail -> Bool
          isLoopPoss trail prevTrail = isIntersect (getPath trail) (getPath prevTrail) && 
                                       ((rotateRight . getDir) trail == getDir prevTrail) 

          isIntersect :: GuardPath -> GuardPath -> Bool
          isIntersect p1 p2 = any (`elem` p2) p1      

guardStep :: Grid -> ObstMap -> Guard -> (Guard, GuardPath, Bool)
guardStep grid@(Grid _ (boundx,boundy)) obstMap guard@(Guard dir _) = (Guard newGuardDir newGuardPos, guardPath, deterOffMap)
    where guardPath = retGuardTrail grid obstMap guard
          newGuardPos = findNewGuardPos dir guardPath
          newGuardDir = rotateRight dir

          findNewGuardPos :: Direction -> ([Coord] -> Coord)
          findNewGuardPos dir
            | alongES dir = last
            | otherwise   = head

          deterOffMap :: Bool
          deterOffMap  = let (newGuardX, newGuardY) = newGuardPos in
                            ((dir == W) && (newGuardX == 0)) ||
                            ((dir == E) && (newGuardX == boundx)) ||
                            ((dir == N) && (newGuardY == 0)) ||
                            ((dir == S) && (newGuardY == boundy))

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
    where insertObst (x,y) (xmap, ymap) = (insertWith insertSortedPl x [y] xmap, insertWith insertSortedPl y [x] ymap)
          
          -- ad-hoc way of ensuring the values in list are sorted. Not general whatsoever. 
          insertSortedPl :: [Int] -> [Int] -> [Int]
          insertSortedPl val@[x] (y:ys)
            | x > y = y : insertSortedPl val ys
            | otherwise = x:y:ys

parseMapString :: Grid -> (Guard, Obstacles)
parseMapString grid = (findGuard grid, findObst grid) 
    where findObst :: Grid -> Obstacles  
          findObst (Grid grid (boundx, boundy)) = [(x,y) | x <- [0..boundx], 
                                                           y <- [0..boundy], 
                                                           let char = (grid !! y) !! x, char /= '.' && char == '#' ]

          findGuard :: Grid -> Guard
          findGuard (Grid grid (boundx, boundy)) = head [ Guard N (x,y) | x <- [0..boundx], 
                                                                          y <- [0..boundy], 
                                                                          let char = (grid !! y) !! x, char /= '.' && char == '^' ]


