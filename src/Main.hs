module Main where

import System.Console.ANSI

main :: IO ()
main = do
  traverse (putStrLn . show) [1..9]
  go 10

go :: Int -> IO ()
go i = do
  cursorUpLine 10
  clearFromCursorToScreenEnd
  traverse (putStrLn . show) [i..i+8]
  go (i+9)
