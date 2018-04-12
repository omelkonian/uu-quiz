{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Backend.Api where

import Data.Proxy
import Servant

import Backend.Model

type ReqDB a = ReqBody '[JSON] a
type PostDB a = Post '[JSON] (Maybe (Key a))
type GetDB a = Get '[JSON] (Maybe a)
type DeleteDB = Delete '[JSON] ()

type QuizApi =
  -- Low-level API
       "quiz" :> ReqDB Quiz :> PostDB Quiz
  :<|> "quiz" :> Capture "quizId" QuizId :> GetDB Quiz
  :<|> "quiz" :> Capture "quizId" QuizId :> DeleteDB
  :<|> "question" :> ReqDB Question :> PostDB Question
  :<|> "question" :> Capture "quizId" QuizId :> Capture "order" Int :> GetDB Question
  :<|> "question" :> Capture "quizId" QuizId :> Capture "order" Int :> DeleteDB
  :<|> "multipleChoice" :> ReqDB MultipleChoice :> PostDB MultipleChoice
  :<|> "multipleChoice" :> Capture "questionId" QuestionId :> Capture "order" Int :> GetDB MultipleChoice
  :<|> "multipleChoice" :> Capture "questionId" QuestionId :> Capture "order" Int :> DeleteDB
  :<|> "openText" :> ReqDB OpenText :> PostDB OpenText
  :<|> "openText" :> Capture "questionId" QuestionId :> GetDB OpenText
  :<|> "openText" :> Capture "questionId" QuestionId :> DeleteDB
  -- High-level API
  :<|> "quizIds" :> Get '[JSON] [QuizId]

quizApi :: Proxy QuizApi
quizApi = Proxy
