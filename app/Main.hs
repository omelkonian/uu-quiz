module Main where

import System.Directory (doesFileExist)
import Control.Monad (unless)

import Frontend.Client  (writeJSFiles)
import Handler          (run)

main :: IO ()
main = do
  jsExists <- doesFileExist "static/api.js"
  unless jsExists $
    writeJSFiles >> putStrLn "Javascript files written"
  putStrLn "Listening on port 8081" >> run 8081 "sqlite.db"
