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

module Backend.Model where

import Data.Aeson
import Database.Persist.TH
import GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Quiz
  description String
  deriving Eq Show Generic

Question
  quizId QuizId
  body String
  order Int
  UniqueQuestion quizId order
  deriving Eq Show Generic

MultipleChoice
  questionId QuestionId
  body String
  order Int
  UniqueChoice questionId order
  deriving Eq Show Generic

OpenText
  questionId QuestionId
  body String
  UniqueOpenText questionId
  deriving Eq Show Generic
|]

-- Flat model.
data FlatQuiz = FlatQuiz
  { description :: String
  , questions   :: [FlatQuestion]
  } deriving (Eq, Show, Generic)
data FlatQuestion = FlatQuestion
  { body   :: String
  , answer :: FlatAnswer
  } deriving (Eq, Show, Generic)
type FlatAnswer = Either FlatOpenText FlatMultipleChoice
type FlatOpenText = String
type FlatMultipleChoice = [String]

-- JSON conversions.
instance FromJSON Quiz
instance ToJSON Quiz
instance FromJSON Question
instance ToJSON Question
instance FromJSON MultipleChoice
instance ToJSON MultipleChoice
instance FromJSON OpenText
instance ToJSON OpenText
instance FromJSON FlatQuiz
instance ToJSON FlatQuiz
instance FromJSON FlatQuestion
instance ToJSON FlatQuestion
