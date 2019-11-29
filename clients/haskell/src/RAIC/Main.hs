module RAIC.Main
  ( run
  ) where

someFunc :: IO ()
someFunc = putStrLn "Hello world!"

run :: IO ()
run = someFunc
