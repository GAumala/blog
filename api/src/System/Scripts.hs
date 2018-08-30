module System.Scripts (runUpdateScriptAtDir) where

import System.Process
import System.IO (IOMode(WriteMode))
import System.Exit (ExitCode)
import GHC.IO.Handle.FD (openFile)

updateScript = proc "bash" ["-eo", "pipefail", "update.sh"]

runUpdateScriptAtDir :: FilePath -> IO ExitCode
runUpdateScriptAtDir workingDir = do
  devNullHandle <- openFile "/dev/null" WriteMode
  (_, _, _, process) <- createProcess updateScript {
    cwd = Just workingDir, 
    std_out = UseHandle devNullHandle, 
    std_err = UseHandle devNullHandle
  }
  waitForProcess process

