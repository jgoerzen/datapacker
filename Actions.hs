{- 
Copyright (C) 2006-2008 John Goerzen <jgoerzen@complete.org>

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.
-}

module Actions(runAction) where
import Types
import Text.Printf
import Data.List

runAction :: RunInfo -> [(Integer, [FilePath])] -> IO ()
runAction ri resultlist =
    case action ri of
      Print -> mapM_ (action_print ri) resultlist
      PrintFull -> action_printfull ri resultlist
      _ -> fail "Action not yet implemented"

formatBin :: RunInfo -> Integer -> String
formatBin ri bin = printf (binFmt ri) bin

action_print :: RunInfo -> (Integer, [FilePath]) -> IO ()
action_print ri (bin, fpl) =
    mapM_ (\fp -> putStrLn ((formatBin ri bin) ++ "\t" ++ fp)) fpl

action_printfull :: RunInfo -> [(Integer, [FilePath])] -> IO ()
action_printfull ri =
    putStr . unlines . map toLine
    where toLine (bin, files) =
              formatBin ri bin ++ "\t" ++ (concat . intersperse "\t" $ files)