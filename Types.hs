{- 
Copyright (C) 2006-2008 John Goerzen <jgoerzen@complete.org>

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.
-}

module Types where

data RunInfo = 
    RunInfo {binSize :: Integer,
             firstBinSize :: Integer,
             preserveOrder :: Bool,
             readNull :: Bool}
    deriving (Eq, Ord, Read, Show)

-- (Size, filepath)
type Result = (Integer, FilePath)
