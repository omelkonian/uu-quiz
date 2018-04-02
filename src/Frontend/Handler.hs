{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Frontend.Handler (frontServer) where

import           Servant
import qualified Text.Blaze.Html5 as H

import Frontend.Api

frontServer :: Server FrontApi
frontServer = return html where
   html :: H.Html
   html = do
       H.b "bar"
       H.i "iik"
