{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Frontend.Handler (frontServer) where

import Control.Applicative        ((<$>))
import Control.Monad              (forM_)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Except
import Database.Persist.Sqlite

import           Data.String
import           Servant
import qualified Text.Blaze.Html5 as H

import Backend.Handler
import Backend.Model
import Frontend.Api

frontServer :: ConnectionPool -> Server FrontApi
frontServer pool =
  html <$> runQuery (selectKeysList [] [Asc QuizId])
  where
    runQuery :: DB a -> ExceptT ServantErr IO a
    runQuery = liftIO . flip runSqlPersistMPool pool

    html :: [QuizId] -> H.Html
    html ids = do
      H.head $ H.title "Quiz Frontend"
      H.body $ do
        H.p "A list of Quiz Ids:"
        H.ul $ forM_ ids (H.li . fromString . show)
