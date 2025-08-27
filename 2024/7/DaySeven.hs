module DaySeven where

import Control.Monad
import Data.List.Split

data Op = Add | Mul | Concat
    deriving (Eq, Show)

main = readFile "./input" >>= \file -> do  let eqlst = map (parseArg . splitOn ":") $ lines file
                                           let totalcalib = sum $ map fst $ filter (uncurry checkCalib) eqlst
                                           print totalcalib 

    where parseArg :: [String] -> (Int, [Int])
          parseArg [calibstr, argstr] = (read calibstr :: Int, (map (\x -> read x :: Int) . words) argstr) 

checkCalib :: Int -> [Int] -> Bool
checkCalib calibSum argLst = calibSum `elem` listPossRes argLst

listPossRes :: [Int] -> [Int]
listPossRes argLst = [eval argLst oplst | oplst <- replicateM (length argLst - 1) [Add, Mul, Concat]]

eval :: [Int] -> [Op] -> Int
eval [x] []      = x
eval (x:y:xs) (op:ops)
    | op == Add   = eval (evalOp Add x y :xs) ops
    | op == Mul   = eval (evalOp Mul x y :xs) ops
    | otherwise   = eval (evalOp Concat x y :xs) ops  

-- Pretty slow.
evalOp :: Op -> Int -> Int -> Int
evalOp Add x y = x + y
evalOp Mul x y = x * y
evalOp Concat x y = (x * 10 ^ (length . show) y) + y


