module RAIC.Main
  ( run
  ) where

-- TODO: Make the record field names consistent
someFunc :: IO ()
someFunc = putStrLn "Hello world!"

-- TODO: Turn on OverloadedStrings, replace [Char] with Text
-- TODO: Consider using 'Vector' over default lists for better performance
run :: IO ()
run = someFunc
