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
import System.FilePath
import System.Posix.Files
import System.Directory
import System.Process
import System.Environment
import System.Exit
import Control.Monad

runAction :: RunInfo -> [(Integer, [FilePath])] -> IO ()
runAction ri resultlist =
    case action ri of
      Print -> action_print ri resultlist
      PrintFull -> action_printfull ri resultlist
      Print0 -> action_print0 ri resultlist
      Hardlink -> action_hardlink ri resultlist
      Symlink -> action_symlink ri resultlist
      Exec x -> action_exec x ri resultlist

formatBin :: RunInfo -> Integer -> String
formatBin ri bin = 
   printf (binFmt ri) bin

action_print :: RunInfo -> [(Integer, [FilePath])] -> IO ()
action_print ri =
    putStr . unlines . concatMap procBin
    where procBin (bin, fp) = map (procLine (formatBin ri bin)) fp
          procLine bin fp = bin ++ "\t" ++ fp

action_printfull :: RunInfo -> [(Integer, [FilePath])] -> IO ()
action_printfull ri =
    putStr . unlines . map toLine
    where toLine (bin, files) =
              formatBin ri bin ++ "\t" ++ (concat . intersperse "\t" $ files)

action_print0 :: RunInfo -> [(Integer, [FilePath])] -> IO ()
action_print0 ri =
    putStr . concatMap toLine
    where toLine (bin, files) = concatMap (fmtFile (formatBin ri bin)) files
          fmtFile binstr file =
              binstr ++ "\0" ++ file ++ "\0"

action_hardlink :: RunInfo -> [(Integer, [FilePath])] -> IO ()
action_hardlink = action_link createLink

action_symlink :: RunInfo -> [(Integer, [FilePath])] -> IO ()
action_symlink = action_link createSymbolicLink

action_link :: (FilePath -> FilePath -> IO ()) -> RunInfo -> [(Integer, [FilePath])] -> IO ()
action_link func ri =
    mapM_ makeLink
    where makeLink (bin, fpl) = 
              mapM_ (makeLink' (formatBin ri bin)) fpl
          makeLink' bin fp =
              if deepLinks ri
                 then do let dirname = bin ++ "/" ++ takeDirectory fp
                         createDirectoryIfMissing True dirname
                         func fp (dirname ++ "/" ++ takeFileName fp)
                 else do createDirectoryIfMissing False bin
                         func fp (bin ++ "/" ++ takeFileName fp)

action_exec :: String -> RunInfo -> [(Integer, [FilePath])] -> IO ()
action_exec cmd ri inp =
    do baseenv <- getEnvironment
       let dshell = case lookup "SHELL" baseenv of
                     Nothing -> "/bin/sh"
                     Just x -> x
       mapM_ (execCommand dshell) inp
    where execCommand sh (bin, fpl) = 
              do ph <- runProcess sh (["-c", cmd, sh, formatBin ri bin] ++ fpl)
                       Nothing Nothing Nothing Nothing Nothing 
                 ec <- waitForProcess ph
                 when (ec /= ExitSuccess)
                      (fail $ "action_exec: command failed on bin " ++ 
                            formatBin ri bin ++ ": " ++ show ec)
