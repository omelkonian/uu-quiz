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

type QuizApi =
       "quiz" :> ReqDB Quiz :> PostDB Quiz
  :<|> "quiz" :> Capture "quizId" QuizId :> GetDB Quiz
  :<|> "question" :> ReqDB Question :> PostDB Question
  :<|> "question" :> Capture "quizId" QuizId :> Capture "order" Int :> GetDB Question
  :<|> "multipleChoice" :> ReqDB MultipleChoice :> PostDB MultipleChoice
  :<|> "multipleChoice" :> Capture "questionId" QuestionId :> Capture "order" Int :> GetDB MultipleChoice
  :<|> "openText" :> ReqDB OpenText :> PostDB OpenText
  :<|> "openText" :> Capture "questionId" QuestionId :> GetDB OpenText

quizApi :: Proxy QuizApi
quizApi = Proxy
