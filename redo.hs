
{-# LANGUAGE StandaloneDeriving #-}

import Control.Monad ( filterM )
import Data.Map.Lazy ( insert, fromList, toList, adjust )
import Data.Maybe ( listToMaybe )
import Debug.Trace ( traceShow )
import System.Directory ( renameFile, removeFile, doesFileExist )
import System.Environment ( getArgs, getEnvironment )
import System.Exit ( ExitCode(..) )
import System.FilePath ( hasExtension, replaceBaseName, takeBaseName )
import System.IO ( hPutStrLn, stderr )
import System.Process ( createProcess, waitForProcess, shell, CreateProcess(..), StdStream(..), CmdSpec(..) )

deriving instance Show CreateProcess
deriving instance Show StdStream
deriving instance Show CmdSpec

main :: IO ()
main = getArgs >>= mapM_ redo

traceShow' arg = traceShow arg arg

redo :: String -> IO ()
redo target = redoPath target >>= maybe printMissing redo'
  where printMissing :: IO ()
        printMissing = error ("No .do file found for target '" ++ target ++ "'")

        tmp = target ++ "---redoing"

        command path = unwords ["sh", path, "0", takeBaseName target, tmp, ">", tmp]

        redo' :: FilePath -> IO ()
        redo' path = do
          oldEnv <- getEnvironment
          let newEnv = toList $ adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv

          (_, _, _, ph) <- createProcess $ traceShow' $ (shell (command path)) { env = Just newEnv }
          exit <- waitForProcess ph
          case exit of
            ExitSuccess -> renameFile tmp target
            ExitFailure code -> do hPutStrLn stderr ("Redo script exited with non-zero exit code: " ++ show code)
                                   removeFile tmp

redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = listToMaybe <$> filterM doesFileExist candidates
  where candidates = (target ++ ".do") : if hasExtension target
                                         then [replaceBaseName target "default" ++ ".do"]
                                         else []

