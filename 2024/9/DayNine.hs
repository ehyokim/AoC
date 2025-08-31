module DayNine where
import Data.Char (digitToInt)

type ArgList = [Int]
type FSPointer = Int
type Index = Int
data ListPointer = LP ArgList FSPointer Index DiskContent
data DiskContent = Data | Free
    deriving (Eq, Show)

-- Could I use a monad for a ListPointer Computation?
-- How can I make this more lazy?


main = readFile "./input" >>= \file -> do   let arg = map digitToInt file
                                            -- print arg
                                            print $ calcCompactedCS arg

calcCompactedCS :: ArgList -> Int
calcCompactedCS = sum . map (uncurry (*)) . calcCompactedFS

calcCompactedFS :: ArgList -> [(Int,Int)]
calcCompactedFS arglst = let blp = LP arglst 0 0 Data
                             elp = LP reversedArgLst endFSPtr (lenArgLst - 1) finalDiskContent
                         in packDisk blp elp
    where reversedArgLst = reverse arglst
        
          lenArgLst = length arglst
          finalDiskContent = if even lenArgLst then Free else Data
          
          endFSPtrOffSet = case finalDiskContent of
                                Free -> head reversedArgLst
                                Data -> 0
          endFSPtr = totalDiskLength arglst - endFSPtrOffSet

packDisk :: ListPointer -> ListPointer -> [(Int,Int)]
packDisk (LP [] _ _ _) _ = error "ArgList for begin list is empty."
packDisk blp@(LP (b:bs) bp bidx bc) elp@(LP _ ep _ _) = case bc of
                                                            Data -> getBlockCS blp elp ++ packDisk (getNextFreeBlockUp blp) elp
                                                            Free -> packDataIntoFS  
    where (takenVal, takenELP) = getDataToPack elp

          packDataIntoFS :: [(Int,Int)]
            | bp >= ep = []
            | b == 0 = packDisk (getNextDataBlockUp blp) elp
            | otherwise = (bp, takenVal) : packDisk (increLP blp) takenELP 

-- Get block CheckSum
getBlockCS :: ListPointer -> ListPointer ->  [(Int,Int)]
getBlockCS blp@(LP (b:bs) bp bidx bc) elp@(LP _ ep _ _)= map (,dataVal) [bp .. (bp + blockLen)]
    where dataVal = bidx `div` 2
          -- Calculate how much of the block we should use for our checksum.    
          blockLen :: Int
          blockLen
            | ep >= bp = min (ep - bp - 1) (b - 1)
            | otherwise = 0

getDataToPack :: ListPointer -> (Int, ListPointer)
getDataToPack lp@(LP _ _ _ Free) = (getDataToPack . getNextDataBlockDown) lp

getDataToPack lp@(LP (el:els) ep idx Data)
    | el == 0 = getDataToPack (LP els ep (idx - 1) Free)
    | el == 1 = (dataVal, (getNextDataBlockDown . getNextFreeBlockDown) lp)
    | otherwise = (dataVal, decreLP lp)
    where dataVal = idx `div` 2

getDataToPack (LP [] _ _ _) = error "Arglist for end list is empty."

-- Decrement the pointer and decrement the number of elements left in argList head. 
decreLP :: ListPointer -> ListPointer
decreLP (LP (el:els) ep idx dc) = LP ((el - 1):els) (ep - 1) idx dc

-- Increment the pointer and decrement the number of elements left in argList head. 
increLP :: ListPointer -> ListPointer
increLP (LP (el:els) ep idx dc) = LP ((el - 1):els) (ep + 1) idx dc

-- Given a data block, skip the data block to get the next free block down. This assumes argList is reversed. 
getNextFreeBlockUp :: ListPointer -> ListPointer
getNextFreeBlockUp (LP _ _ _ Free) = error "getNextFreeBlockUp should have Data ListPointer as input."
getNextFreeBlockUp (LP [] _ _ _) = error "ListArg is empty for getNextFreeBlockUp."
getNextFreeBlockUp (LP (el:els) ep idx Data) = LP els (ep + el) (idx + 1) Free

-- Given a free block, skip the free block to get the next data block down.
getNextDataBlockUp :: ListPointer -> ListPointer
getNextDataBlockUp (LP _ _ _ Data) = error "getNextDataBlockUp should have Free ListPointer as input."
getNextDataBlockUp (LP [] _ _ _) = error "ListArg is empty for getNextDataBlockUp."
getNextDataBlockUp (LP (el:els) ep idx Free) = LP els (ep + el) (idx + 1) Data


-- Given a data block, skip the data block to get the next free block down. This assumes argList is reversed. 
getNextFreeBlockDown :: ListPointer -> ListPointer
getNextFreeBlockDown (LP _ _ _ Free) = error "getNextFreeBlockDown should have Data ListPointer as input."
getNextFreeBlockDown (LP [] _ _ _) = error "ListArg is empty for getNextFreeBlockDown."
getNextFreeBlockDown (LP (el:els) ep idx Data) = LP els (ep - el) (idx - 1) Free

-- Given a free block, skip the free block to get the next data block down.
getNextDataBlockDown :: ListPointer -> ListPointer
getNextDataBlockDown (LP _ _ _ Data) = error "getNextDataBlockDown should have Free ListPointer as input."
getNextDataBlockDown (LP [] _ _ _) = error "ListArg is empty for getNextDataBlockDown."
getNextDataBlockDown (LP (el:els) ep idx Free) = LP els (ep - el) (idx - 1) Data

totalDiskLength :: ArgList -> Int
totalDiskLength = sum 

