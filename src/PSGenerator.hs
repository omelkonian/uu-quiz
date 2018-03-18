{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Data.Proxy
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
import           Servant.API
import           Servant.PureScript
import           Servant.Subscriber.Subscribable

import           Quiz.Model
import           Quiz.Api

-- | We have been lazy and defined our types in the WebAPI module,
--   we use this opportunity to show how to create a custom bridge moving those
--   types to Counter.ServerTypes.
fixTypesModule :: BridgePart
fixTypesModule = do
  typeModule ^== "Quiz.Api"
  t <- view haskType
  TypeInfo (_typePackage t) "Quiz.ServerTypes" (_typeName t) <$> psTypeParameters

myBridge :: BridgePart
myBridge = defaultBridge <|> fixTypesModule

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
  languageBridge _ = buildBridge myBridge

myTypes :: [SumType 'Haskell]
myTypes =  [
            mkSumType (Proxy :: Proxy Quiz)
          , mkSumType (Proxy :: Proxy Question)
          , mkSumType (Proxy :: Proxy MultipleChoice)
          , mkSumType (Proxy :: Proxy OpenText)
          ]

mySettings :: Settings
mySettings = addReaderParam "AuthToken" defaultSettings & apiModuleName .~ "Quiz.Api"

main :: IO ()
main = do
  let frontEndRoot = "frontend/src"
  writeAPIModuleWithSettings mySettings frontEndRoot myBridgeProxy quizApi
  writePSTypes frontEndRoot (buildBridge myBridge) myTypes
