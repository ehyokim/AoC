module DayFour where
import Prelude hiding (Word)
import GHC.Utils.Misc

type Coord = (Int, Int)
type Word = String 
type Anchor = Char
type Candidate = [Coord]
data WordSearch = WS [String] (Int, Int) Predicate Anchor
type Predicate = WordSearch -> Candidate -> Bool
type StrCheckFunc = WordSearch -> Coord -> Int

main = readFile "./input" >>= \file -> do  let grid = lines file
                                           let wsxmas = WS grid (length (head grid) - 1, length grid - 1) xmasPred 'X'
                                           let wsmascross = WS grid (length (head grid) - 1, length grid - 1) masCrossPred 'A'
                                           print $ checkWSXMAS wsxmas
                                           print $ checkWSMASCross wsmascross


checkWSXMAS :: WordSearch -> Int
checkWSXMAS ws@(WS _ (boundx, boundy) _ _) = sum [checkCoord ws funcs (x,y) | x <- [0..boundx], y <- [0..boundy]]
    where funcs = [checkDiag, checkVert, checkHori]

checkWSMASCross :: WordSearch -> Int
checkWSMASCross ws@(WS _ (boundx, boundy) _ _) = sum [checkCoord ws funcs (x,y) | x <- [0..boundx], y <- [0..boundy]]
    where funcs = [checkMASCross]

checkCoord :: WordSearch -> [StrCheckFunc] -> Coord -> Int
checkCoord ws@(WS _ _ _ ch) fs coord
    | charAtCoord == ch     = sum $ map (($ coord) . ($ ws)) fs
    | otherwise             = 0
    where charAtCoord = getCharFromWS ws coord

checkMASCross :: WordSearch -> Coord -> Int
checkMASCross ws (x,y) = let cand = [[(x-1,y-1), (x+1,y-1), (x,y), (x-1,y+1), (x+1,y+1)]] in
                            checkCand ws cand
                             
checkDiag :: WordSearch -> Coord -> Int
checkDiag ws (x,y) = let posDirs = [[(x+i,y+i) | i <- [0..3]], [(x-i,y+i)| i <- [0..3]]]
                         negDirs = [[(x+i,y-i) | i <- [0..3]], [(x-i,y-i)| i <- [0..3]]] 
                     in  
                        checkCand ws (posDirs ++ negDirs)

checkVert :: WordSearch -> Coord -> Int
checkVert ws (x,y) = let cand = [[(x,y+i) | i <- [0..3]], [(x,y-i) | i <- [0..3]]] in
                        checkCand ws cand

checkHori :: WordSearch -> Coord -> Int
checkHori ws (x,y) = let cand = [[(x+i,y) | i <- [0..3]], [(x+i,y) | i <- [0, (-1)..(-3)]]] in
                        checkCand ws cand 

checkCand :: WordSearch -> [Candidate] -> Int
checkCand ws@(WS _ bounds _ _) candlst = count id  $ map checkValidString candlst
    where  checkValidString :: Candidate -> Bool
           checkValidString cand
                | not $ checkValidRange bounds cand = False
                | otherwise = checkString ws cand

checkString :: WordSearch -> Candidate -> Bool
checkString ws@(WS _ _ pred _) = pred ws

xmasPred :: WordSearch -> Candidate -> Bool
xmasPred ws lst = [getCharFromWS ws coord | coord <- lst] == "XMAS"

masCrossPred :: WordSearch -> Candidate -> Bool
masCrossPred ws lst
    | not $ (tl == 'M' && br == 'S') || (tl == 'S' && br == 'M') = False
    | not $ (tr == 'M' && bl == 'S') || (tr == 'S' && bl == 'M') = False
    | otherwise                                                  = True 
    where [tl, tr, _, bl, br] = [getCharFromWS ws coord | coord <- lst]
                       
getCharFromWS :: WordSearch -> Coord -> Char
getCharFromWS (WS grid _ _ _) (x,y) = (grid !! y) !! x

checkValidRange ::  (Int, Int) -> Candidate -> Bool
checkValidRange bounds = all (checkDim bounds)

checkDim :: (Int, Int) -> Coord -> Bool
checkDim (boundx, boundy) (x,y) 
    | (x < 0) || (x > boundx) = False
    | (y < 0) || (y > boundy) = False
    | otherwise               = True