module DayNinePartTwo where
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Control.Monad 
import Data.Char (digitToInt)
import DayNine hiding (main)

type ExpandedList = [Int]
type BlockIdxList = [(Int,Int)]
type FSListRep = (BlockIdxList, BlockIdxList) 


main = readFile "./input" >>= \file -> do  let arglst = map digitToInt file
                                           let blkIdxLsts = constBlockIdxLsts arglst
                                           vec <- constVector arglst (totalDiskLength arglst)
                                           swappedvec <- swapFS blkIdxLsts vec
                                           --swapBlocks (40,2) (2,3) vec
                                           --ivec <- U.freeze vec
                                           -- print ivec
                                           -- print swappedvec
                                           print $ calcCheckSum swappedvec


calcCheckSum :: U.Vector Int -> Int
calcCheckSum vec = U.sum $ U.zipWith (*) (U.enumFromN 0 (U.length vec)) vec 

-- Really slow. Please profile this code. 
swapFS :: PrimMonad m => FSListRep -> MU.MVector (PrimState m) Int -> m (U.Vector Int)
swapFS ([], _) vec = do U.freeze vec 
swapFS ((d:ds), freelst) vec = case searchFreeBlock d freelst of
                                    Nothing -> swapFS (ds, freelst) vec
                                    Just (freeBlock, updateFreeLst) ->  do swapBlocks d freeBlock vec
                                                                           swapFS (ds, updateFreeLst) vec
    where searchFreeBlock :: (Int,Int) -> BlockIdxList -> Maybe ((Int, Int), BlockIdxList)
          searchFreeBlock _ [] = Nothing
          searchFreeBlock d@(dataidx, datalen) (fb@(freeidx, freelen):fs)
            | freeidx >= dataidx = Nothing
            | datalen == freelen = Just (fb, fs)
            | datalen < freelen = Just (fb, (freeidx + datalen, freelen - datalen):fs)
            | otherwise = ((fb:) <$>) <$> searchFreeBlock d fs

constBlockIdxLsts :: ArgList -> FSListRep
constBlockIdxLsts arglst = constFunc ([],[]) 0 (zip [0..] arglst)
    where constFunc :: (BlockIdxList, BlockIdxList) -> Int -> [(Int, Int)] -> (BlockIdxList, BlockIdxList)
          constFunc lsts _ [] = lsts
          constFunc (datalst, freelst) absidx ((i,len):zs)
            | even i = constFunc ((absidx, len) : datalst, freelst) (absidx + len) zs
            | otherwise = constFunc (datalst, freelst ++ [(absidx, len)]) (absidx + len) zs

swapBlocks :: (PrimMonad m, MU.Unbox a) => 
      (Int, Int) -> 
      (Int, Int) -> 
      MU.MVector (PrimState m) a -> 
      m ()
swapBlocks (b1idx, b1len) (b2idx, _) vec = do zipWithM_ (MU.swap vec) [b1idx .. (b1idx + b1len - 1)] [b2idx..]

constVector :: PrimMonad m =>
       ExpandedList -> 
       Int -> 
       m (MU.MVector (PrimState m) Int)
constVector arglst len = do nvec <- MU.unsafeNew len
                            let loop [] _  = return nvec 
                                loop ((idx, len): ls) absidx = do 
                                          zipWithM_ (MU.unsafeWrite nvec) [absidx .. (absidx + len - 1)] (repeat numToFill)
                                          loop ls (absidx + len) 
                                    where numToFill = if even idx then (idx `div` 2) else 0
                            loop (zip [0..] arglst) 0