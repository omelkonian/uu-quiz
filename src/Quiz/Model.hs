{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Quiz.Model where

import           Data.Aeson
import           Database.Persist.TH
import           GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Quiz
  description String
  deriving Show Generic

Question
  quizId QuizId
  body String
  order Int
  UniqueQuestion quizId order
  deriving Show Generic

MultipleChoice
  questionId QuestionId
  body String
  order Int
  UniqueChoice questionId order
  deriving Show Generic

OpenText
  questionId QuestionId
  body String
  UniqueOpenText questionId
  deriving Show Generic
|]

instance FromJSON Quiz
instance ToJSON Quiz
instance FromJSON Question
instance ToJSON Question
instance FromJSON MultipleChoice
instance ToJSON MultipleChoice
instance FromJSON OpenText
instance ToJSON OpenText
