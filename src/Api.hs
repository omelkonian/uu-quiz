{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Backend.Api  (QuizApi)
import Frontend.Api (FrontApi)
import Servant

type FullApi = QuizApi :<|> FrontApi :<|> Raw

fullApi :: Proxy FullApi
fullApi = Proxy
