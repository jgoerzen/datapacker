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
import Data.BinPacking

scan :: RunInfo -> [FilePath] -> IO [[Result]]
scan ri fplist =
    do sizes <- (liftM concat $ mapM getSize fplist)
       let func = if preserveOrder ri then packByOrder else packLargeFirst
       case func bins sizes of
         Left x -> fail (show x)
         Right x -> return x
    where getSize f = 
              do s <- getFileStatus f
                 if isRegularFile s
                    then return [(fromIntegral (fileSize s), f)]
                    else do warningM "scan" $ "Warning: file " ++ f ++ " is not a regular file; skipping"
                            return []
                    
          bins = firstBinSize ri : repeat (binSize ri)
