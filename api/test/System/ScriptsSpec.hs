{-# LANGUAGE OverloadedStrings #-}


module System.ScriptsSpec where

import Test.Hspec
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.TestScripts (dirWithBadScript, dirWithGoodScript)
import System.Scripts (runUpdateScriptAtDir)

spec = 
  describe "runUpdateScriptAtDir" $ do
    it "returns ExitSuccess if the script exits without errors" $ do
      exitCode <- runUpdateScriptAtDir dirWithGoodScript

      exitCode `shouldBe` ExitSuccess
      
      
    it "returns ExitFailure if the script threw an error during execution" $ do
      exitCode <- runUpdateScriptAtDir dirWithBadScript

      exitCode `shouldBe` (ExitFailure 2)
      

