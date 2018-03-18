module Main where

import Quiz.Handler (run)

main :: IO ()
main = run 8081 "sqlite.db"
