{-# LANGUAGE CPP #-}

module System.TestScripts (dirWithBadScript, dirWithGoodScript) where

import System.FilePath

dirWithGoodScript = (takeDirectory __FILE__) </> "good"
dirWithBadScript = (takeDirectory __FILE__) </> "bad"
