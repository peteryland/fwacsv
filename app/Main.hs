module Main where

import System.Environment(getArgs)
import Lib(processFiles)

main :: IO ()
main = getArgs >>= processFiles
