{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Backend.Handler where

import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Database.Persist.Sqlite
import Prelude                      hiding ((^^))
import Servant

import Backend.Api
import Backend.Model

type DB a = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a

quizServer :: ConnectionPool -> Server QuizApi
quizServer pool =
       l1 quizAdd           :<|> l1 quizGet           :<|> l1 quizDel
  :<|> l1 questionAdd       :<|> l2 questionGet       :<|> l2 questionDel
  :<|> l1 multipleChoiceAdd :<|> l2 multipleChoiceGet :<|> l2 multipleChoiceDel
  :<|> l1 openTextAdd       :<|> l1 openTextGet       :<|> l1 openTextDel
  :<|> l0 getAllQuizIds
  where
    l0 :: DB a -> ExceptT ServantErr IO a
    l0 = liftIO . flip runSqlPersistMPool pool
    l1 :: (b -> DB a) -> (b -> ExceptT ServantErr IO a)
    l1 f b = liftIO $ runSqlPersistMPool (f b) pool
    l2 :: (b -> c -> DB a) -> (b -> c -> ExceptT ServantErr IO a)
    l2 f b c = liftIO $ runSqlPersistMPool (f b c) pool

    getAllQuizIds :: DB [QuizId]
    getAllQuizIds =
      selectKeysList [] [Asc QuizId]

    quizAdd :: Quiz -> DB (Maybe QuizId)
    quizAdd newQuiz = Just <$> insert newQuiz
    quizGet :: QuizId -> DB (Maybe Quiz)
    quizGet = get
    quizDel :: QuizId -> DB ()
    quizDel = delete

    questionAdd :: Question -> DB (Maybe QuestionId)
    questionAdd newQuestion = do
      exists <- getBy $ UniqueQuestion (questionQuizId newQuestion) (questionOrder newQuestion)
      case exists of
        Nothing -> Just <$> insert newQuestion
        Just _  -> return Nothing
    questionGet :: QuizId -> Int -> DB (Maybe Question)
    questionGet quizId order = do
      mQuestion <- getBy $ UniqueQuestion quizId order
      return $ entityVal <$> mQuestion
    questionDel :: QuizId -> Int -> DB ()
    questionDel quizId order =
      deleteBy $ UniqueQuestion quizId order

    multipleChoiceAdd :: MultipleChoice -> DB (Maybe MultipleChoiceId)
    multipleChoiceAdd newChoice = do
      exists <- getBy $ UniqueChoice (multipleChoiceQuestionId newChoice) (multipleChoiceOrder newChoice)
      case exists of
        Nothing -> Just <$> insert newChoice
        Just _  -> return Nothing
    multipleChoiceGet :: QuestionId -> Int -> DB (Maybe MultipleChoice)
    multipleChoiceGet questionId order = do
      mChoice <- getBy $ UniqueChoice questionId order
      return $ entityVal <$> mChoice
    multipleChoiceDel :: QuestionId -> Int -> DB ()
    multipleChoiceDel questionId order =
      deleteBy $ UniqueChoice questionId order

    openTextAdd :: OpenText -> DB (Maybe OpenTextId)
    openTextAdd newOpen = do
      exists <- getBy $ UniqueOpenText (openTextQuestionId newOpen)
      case exists of
        Nothing -> Just <$> insert newOpen
        Just _  -> return Nothing
    openTextGet :: QuestionId -> DB (Maybe OpenText)
    openTextGet questionId = do
      mOpen <- getBy $ UniqueOpenText questionId
      return $ entityVal <$> mOpen
    openTextDel :: QuestionId -> DB ()
    openTextDel questionId =
      deleteBy $ UniqueOpenText questionId
