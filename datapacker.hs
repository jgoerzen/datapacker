{- 
Copyright (C) 2006-2008 John Goerzen <jgoerzen@complete.org>

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.
-}

{- |
   Module     : Main
   Copyright  : Copyright (C) 2008 John Goerzen
   License    : GNU GPL, version 3 or above; see COPYRIGHT for details

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}

import System.Log.Logger
import System.Log.Handler.Simple
import System.IO(stderr)
import System.Console.GetOpt.Utils
import System.Console.GetOpt
import System.Environment
import Data.Quantity
import Data.List
import System.Exit
import Control.Monad
import Types
import Scan
import Data.List.Utils(split)
import Actions

main :: IO ()
main = 
    do updateGlobalLogger "" (setLevel INFO)
       argv <- getArgs
       case getOpt RequireOrder options argv of
         (o, n, []) -> worker o n
         (_, _, errors) -> usageerror (concat errors) -- ++ usageInfo header options)

options :: [OptDescr (String, String)]       
options = [
           Option "0" ["null"] (NoArg ("0", ""))
                  "Input items terminated by null character",
           Option "a" ["action"] (ReqArg (stdRequired "a") "ACTION")
                  "Give action for output.  Options are:\n\
                  \print     print each record with a newline\n\
                  \          after [default]\n\
                  \printfull print one line for each bin\n\
                  \print0    print each record with NULL after\n\
                  \exec:CMD  Execute CMD in the shell for each\n\
                  \record\n\
                  \hardlink  Hard link items into bins\n\
                  \symlink   Symlink items into bins",
           Option "b" ["binfmt"] (ReqArg (stdRequired "b") "FORMAT")
                  "Gives bin name format in printf style.\n\
                  \Tip: this can include a directory.\n\
                  \default: %03d",
           Option "d" ["debug"] (NoArg ("d", "")) "Enable debugging",
           Option "D" ["deep-links"] (NoArg ("D", "")) "Enable deep bin directories",
           Option "p" ["preserve-order"] (NoArg ("p", ""))
                  "Don't reorder files for maximum packing",
           Option "s" ["size"] (ReqArg (stdRequired "s") "SIZE")
                  "Size of each output bin",
           Option "S" ["size-first"] (ReqArg (stdRequired "S") "SIZE")
                  "Override size of first output bin",
           Option "" ["sort"] (NoArg ("sort", "")) "Sort input; useless without -p",
           Option "" ["help"] (NoArg ("help", "")) "Display this help"]

worker :: [(String, String)] -> [FilePath] -> IO ()
worker args files =
    do when (lookup "help" args == Just "") $ usageerror ""
       when (lookup "d" args == Just "") 
            (updateGlobalLogger "" (setLevel DEBUG))
       handler <- streamHandler stderr DEBUG
       updateGlobalLogger "" (setHandlers [handler])
       
       runinfo <- case parseArgs args of
                       Left x -> usageerror x
                       Right x -> return x

       when (files == [])
            (usageerror "One or more files, or \"-\", must be specified")

       files_scan <- if files == ["-"]
                        then readFileList (readNull runinfo)
                        else return files

       let listToProc = if sortFiles runinfo then sort files_scan else files_scan

       results <- scan runinfo listToProc
       let numberedResults = zip [1..] (map (map snd) results)
       runAction runinfo numberedResults

readFileList :: Bool -> IO [FilePath]
readFileList nullsep =
    do c <- getContents
       return (splitfunc c)
    where splitfunc 
              | nullsep = filter (/= "") . split "\0"
              | otherwise = lines

parseArgs :: [(String, String)] -> Either String RunInfo
parseArgs args =
    do size <- case lookup "s" args of
                 Nothing -> error "Missing required argument --size"
                 Just x -> parseNumInt binaryOpts True x
       first <- case lookup "S" args of
                  Nothing -> return size
                  Just x -> parseNumInt binaryOpts True x
       let po = case lookup "p" args of
                  Nothing -> False
                  Just _ -> True
       let n = case lookup "0" args of
                 Nothing -> False
                 Just _ -> True
       let b = case lookup "b" args of
                 Nothing -> "%03d"
                 Just x -> x
       let deeplinks = case lookup "D" args of
                        Nothing -> False
                        Just _ -> True
       let dosort = case lookup "sort" args of
                      Nothing -> False
                      Just _ -> True
       a <- case lookup "a" args of
              Nothing -> return Print
              Just "print" -> return Print
              Just "printfull" -> return PrintFull
              Just "print0" -> return Print0
              Just "hardlink" -> return Hardlink
              Just "symlink" -> return Symlink
              Just x -> 
                  if "exec:" `isPrefixOf` x
                     then return (Exec (drop 5 x))
                     else error $ "Unknown action: " ++ show x
       return $ RunInfo {binSize = size, firstBinSize = first,
                         preserveOrder = po, readNull = n, binFmt = b,
                         action = a, deepLinks = deeplinks, sortFiles = dosort}

usageerror :: String -> IO t
usageerror errormsg =
    do putStrLn errormsg
       putStrLn (usageInfo header options)
       putStrLn "If the single value \"-\" is given for inputfiles, the list of files"
       putStrLn "is read from stdin."
       exitFailure

header :: String
header = "\nUsage: datapacker [options] --size=n inputfiles\n\n\
         \Available options are:\n"
