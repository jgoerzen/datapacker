{- 
Copyright (C) 2006-2008 John Goerzen <jgoerzen@complete.org>

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.
-}

module Scan(scan) where
import Types
import Control.Monad(liftM)
import System.Posix.Files
import System.Log.Logger

scan :: RunInfo -> [FilePath] -> IO [[Result]]
scan ri fplist =
    do sizes <- (liftM concat $ mapM getSize fplist)
       if preserveOrder ri
          then return $ binify_po bins sizes
--          else binify_opt bins sizes
          else fail "Not done"
    where getSize f = 
              do s <- getFileStatus f
                 if isRegularFile s
                    then return [(fromIntegral (fileSize s), f)]
                    else do warningM "scan" $ "Warning: file " ++ f ++ " is not a regular file; skipping"
                            return []
                    
          bins = firstBinSize ri : repeat (binSize ri)

binify_po :: (Num s, Ord s) => [s] -> [(s, a)] -> [[(s, a)]]
binify_po _ [] = []
binify_po (thisbinsize:otherbins) sizes =
    let thisset = fillBin 0 sizes
        fillBin accumsize ((s, o):xs) 
            | s > thisbinsize = error $ "Size " ++ show s ++ " greater than bin size " ++ show thisbinsize
            | s + accumsize > thisbinsize = []
            | otherwise = (s, o) : fillBin (accumsize + s) xs
        in thisset : binify_po otherbins (drop (length thisset) sizes)

