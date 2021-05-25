module Main where

import Text
import System.Console.ANSI
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  doc <- getDoc args
  putStrLn $ show doc
  putStrLn $ fromDoc doc
