{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Quiz.Handler where


import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Logger     (runStderrLoggingT)
import           Data.String.Conversions  (cs)

import           Database.Persist.Sqlite  (ConnectionPool, createSqlitePool,
                                           entityVal, get, getBy, insert,
                                           runMigration, runSqlPersistMPool,
                                           runSqlPool)
import           Network.Wai.Handler.Warp as Warp
import           Servant

import           Quiz.Api
import           Quiz.Model

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
fullServer pool = quizServer pool :<|> serveDirectory "frontend/dist/"

quizServer :: ConnectionPool -> Server QuizApi
quizServer pool =
       quizAddH :<|> quizGetH
  :<|> questionAddH :<|> questionGetH
  :<|> multipleChoiceAddH :<|> multipleChoiceGetH
  :<|> openTextAddH :<|> openTextGetH
  where
    quizAddH = liftIO . quizAdd
    quizGetH = liftIO . quizGet

    questionAddH = liftIO . questionAdd
    questionGetH quizId order = liftIO $ questionGet quizId order

    multipleChoiceAddH = liftIO . multipleChoiceAdd
    multipleChoiceGetH questionId order = liftIO $ multipleChoiceGet questionId order

    openTextAddH = liftIO . openTextAdd
    openTextGetH = liftIO . openTextGet

    quizAdd :: Quiz -> IO (Maybe QuizId)
    quizAdd newQuiz = flip runSqlPersistMPool pool $
      Just <$> insert newQuiz
    quizGet :: QuizId -> IO (Maybe Quiz)
    quizGet quizId = flip runSqlPersistMPool pool $
      get quizId

    questionAdd :: Question -> IO (Maybe QuestionId)
    questionAdd newQuestion = flip runSqlPersistMPool pool $ do
      exists <- getBy $ UniqueQuestion (questionQuizId newQuestion) (questionOrder newQuestion)
      case exists of
        Nothing -> Just <$> insert newQuestion
        Just _  -> return Nothing
    questionGet :: QuizId -> Int -> IO (Maybe Question)
    questionGet quizId order = flip runSqlPersistMPool pool $ do
      mQuestion <- getBy $ UniqueQuestion quizId order
      return $ entityVal <$> mQuestion

    multipleChoiceAdd :: MultipleChoice -> IO (Maybe MultipleChoiceId)
    multipleChoiceAdd newChoice = flip runSqlPersistMPool pool $ do
      exists <- getBy $ UniqueChoice (multipleChoiceQuestionId newChoice) (multipleChoiceOrder newChoice)
      case exists of
        Nothing -> Just <$> insert newChoice
        Just _  -> return Nothing
    multipleChoiceGet :: QuestionId -> Int -> IO (Maybe MultipleChoice)
    multipleChoiceGet questionId order = flip runSqlPersistMPool pool $ do
      mChoice <- getBy $ UniqueChoice questionId order
      return $ entityVal <$> mChoice

    openTextAdd :: OpenText -> IO (Maybe OpenTextId)
    openTextAdd newOpen = flip runSqlPersistMPool pool $ do
      exists <- getBy $ UniqueOpenText (openTextQuestionId newOpen)
      case exists of
        Nothing -> Just <$> insert newOpen
        Just _  -> return Nothing
    openTextGet :: QuestionId -> IO (Maybe OpenText)
    openTextGet questionId = flip runSqlPersistMPool pool $ do
      mOpen <- getBy $ UniqueOpenText questionId
      return $ entityVal <$> mOpen
