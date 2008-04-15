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
import System.IO(stdout)
import System.Console.GetOpt.Utils
import System.Console.GetOpt
import Data.List
import System.Exit
import Control.Monad

main = 
    do updateGlobalLogger "" (setLevel INFO)
       argv <- getArgs
       case getOpt RequireOrder options argv of
         (o, n, []) -> worker o n
         (_, _, errors) -> usageerror (concat errors) -- ++ usageInfo header options)
       
options = [Option "d" ["debug"] (NoArg ("d", "")) "Enable debugging",
           Option "p" ["preserve-order"] (NoArg ("p", ""))
                  "Don't reorder files for maximum packing",
           Option "s" ["size"] (ReqArg (stdRequired "s") "SIZE")
                  "Size of each output bin",
           Option "S" ["size-first"] (ReqArg (stdRequired "S") "SIZE")
                  "Override size of first output bin",
           Option "" ["help"] (NoArg ("help", "")) "Display this help"]

worker args othern =
    do when (lookup "help" args == Just "") $ usageerror ""
       when (lookup "d" args == Just "") 
            (updateGlobalLogger "" (setLevel DEBUG))
       handler <- streamHandler stdout DEBUG
       updateGlobalLogger "" (setHandlers [handler])
       
       initDirs
       let commandname = head cmdargs
       case lookup commandname allCommands of
         Just command -> 
             do cp <- loadCP 
                dbh <- connect
                handleSqlError $ execcmd command (tail cmdargs) 
                                   (GlobalInfo {gcp = cp, gdbh = dbh})
                disconnect dbh
         Nothing -> usageerror ("Invalid command name " ++ commandname)
       where cmdargs = case commandargs of
                         [] -> ["fetch"]
                         x -> x

usageerror errormsg =
    do putStrLn errormsg
       putStrLn (usageInfo header options)
       exitFailure

header = "Usage: datapacker [options] inputfiles\n\n\
         \Available options are:\n"
