module AppSpec where

import Control.Exception        (ErrorCall (..), throwIO)
import Data.Maybe               (fromJust, isJust)
import Network.HTTP.Client      (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp
import Servant.Client
import Test.Hspec
import Test.Mockery.Directory

import Backend.Model
import Frontend.Client
import Handler         (mkApp)

-- | Test all low-level endpoints.
spec :: Spec
spec =
  around withApp $ do
    let q1 = Quiz "some desc"
    describe "/quiz" $ do
      it "make a quiz" $ \port -> do
        qId <- port <@ mkQuiz q1
        isJust qId ?= True
      it "get a quiz" $ \port -> do
        qId <- port ~@ mkQuiz q1
        q <- port <@ getQuiz qId
        q ?= Just q1
    let qu1 qId = Question qId "What?" 1
    describe "/question" $ do
      it "make a question" $ \port -> do
        qId <- port ~@ mkQuiz q1
        quId <- port <@ mkQuestion (qu1 qId)
        isJust quId ?= True
      it "get a question" $ \port -> do
        qId <- port ~@ mkQuiz q1
        _ <- port <@ mkQuestion (qu1 qId)
        qu <- port ~@ getQuestion qId 1
        qu ?= qu1 qId
    let mc1 quId = MultipleChoice quId "Choice A" 1
    describe "/multipleChoice" $ do
      it "make a multiple choice question" $ \port -> do
        qId <- port ~@ mkQuiz q1
        quId <- port ~@ mkQuestion (qu1 qId)
        mc <- port <@ mkMultipleChoice (mc1 quId)
        isJust mc ?= True
      it "get a multiple choice question" $ \port -> do
        qId <- port ~@ mkQuiz q1
        quId <- port ~@ mkQuestion (qu1 qId)
        _ <- port ~@ mkMultipleChoice (mc1 quId)
        mc <- port ~@ getMultipleChoice quId 1
        mc ?= mc1 quId
    let ot1 quId = OpenText quId "Some text"
    describe "/openText" $ do
      it "make a multiple choice question" $ \port -> do
        qId <- port ~@ mkQuiz q1
        quId <- port ~@ mkQuestion (qu1 qId)
        ot <- port <@ mkOpenText (ot1 quId)
        isJust ot ?= True
      it "get a multiple choice question" $ \port -> do
        qId <- port ~@ mkQuiz q1
        quId <- port ~@ mkQuestion (qu1 qId)
        _ <- port ~@ mkOpenText (ot1 quId)
        ot <- port ~@ getOpenText quId
        ot ?= ot1 quId

withApp :: (Port -> IO a) -> IO a
withApp action =
  inTempDirectory $ do
    app <- mkApp "sqlite.db"
    testWithApplication (return app) action

(?=) :: (Eq a, Show a) => a -> a -> Expectation
a ?= b = return a `shouldReturn` b

tryJust, (~@) :: Port -> ClientM (Maybe a) -> IO a
(~@) = tryJust
tryJust port = fmap fromJust . try port

try, (<@) :: Port -> ClientM a -> IO a
(<@) = try
try port action = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" port ""
  result <- runClientM action (ClientEnv manager baseUrl)
  case result of
    Left err -> throwIO $ ErrorCall $ show err
    Right a  -> return a
