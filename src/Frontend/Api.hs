{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Frontend.Api where

import           Data.Proxy
import           Servant
import           Servant.HTML.Blaze
import qualified Text.Blaze.Html5   as H

type FrontApi = Get '[HTML] H.Html

frontApi :: Proxy FrontApi
frontApi = Proxy
