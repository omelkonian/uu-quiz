module Main where

import Handler (run)

main :: IO ()
main = putStrLn "Listening on port 8081" >> run 8081 "sqlite.db"
