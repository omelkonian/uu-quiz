{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Handler where

import Control.Monad.Logger    (runStderrLoggingT)
import Data.String.Conversions (cs)
import Database.Persist.Sqlite (ConnectionPool, createSqlitePool, runMigration,
                                runSqlPool)

import qualified Network.Wai.Handler.Warp as Warp
import           Servant

import Api
import Backend.Handler
import Backend.Model    (migrateAll)
import Frontend.Handler

run :: Int -> FilePath -> IO ()
run port sqliteFile =
  Warp.run port =<< mkApp sqliteFile

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ createSqlitePool (cs sqliteFile) 5
  runSqlPool (runMigration migrateAll) pool
  return $ app pool

app :: ConnectionPool -> Application
app pool = serve fullApi $ fullServer pool

fullServer :: ConnectionPool -> Server FullApi
fullServer pool = quizServer pool
             :<|> frontServer pool
             :<|> serveDirectory "static/"
