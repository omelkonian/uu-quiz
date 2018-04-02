module Frontend.Client where

import Servant
import Servant.Client

import Backend.Model
import Backend.Api

import Data.Text as T (Text)
import Data.Text.IO as T (writeFile, readFile)
import Language.Javascript.JQuery
import Servant.JS

-- | Derive client functions.
mkQuiz            :: Quiz              -> ClientM (Maybe QuizId)
getQuiz           :: QuizId            -> ClientM (Maybe Quiz)
mkQuestion        :: Question          -> ClientM (Maybe QuestionId)
getQuestion       :: QuizId -> Int     -> ClientM (Maybe Question)
mkMultipleChoice  :: MultipleChoice    -> ClientM (Maybe MultipleChoiceId)
getMultipleChoice :: QuestionId -> Int -> ClientM (Maybe MultipleChoice)
mkOpenText        :: OpenText          -> ClientM (Maybe OpenTextId)
getOpenText       :: QuestionId        -> ClientM (Maybe OpenText)
mkQuiz :<|> getQuiz :<|> mkQuestion :<|> getQuestion :<|> mkMultipleChoice
  :<|> getMultipleChoice :<|> mkOpenText :<|> getOpenText = client quizApi

-- Generate Javascript files.
apiJS1 :: Text
apiJS1 = jsForAPI quizApi jquery

writeJSFiles :: IO ()
writeJSFiles = do
  T.writeFile "static/api.js" apiJS1
  jq <- T.readFile =<< Language.Javascript.JQuery.file
  T.writeFile "static/jq.js" jq
