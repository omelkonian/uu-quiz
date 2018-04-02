module Main where

import Handler (run)
import Frontend.Client (writeJSFiles)

main :: IO ()
main = do
  writeJSFiles >> putStrLn "Javascript files written"
  putStrLn "Listening on port 8081" >> run 8081 "sqlite.db"
