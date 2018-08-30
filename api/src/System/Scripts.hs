module System.Scripts (runUpdateScriptAtDir) where

import System.Process
import System.Exit (ExitCode)

updateScript = proc "bash" ["-eo", "pipefail", "update.sh"]

runUpdateScriptAtDir :: FilePath -> IO ExitCode
runUpdateScriptAtDir workingDir = do
  (_, _, _, process) <- createProcess updateScript {
    cwd = Just workingDir, 
    std_in = NoStream, 
    std_out = CreatePipe, 
    std_err = CreatePipe
  }
  waitForProcess process

