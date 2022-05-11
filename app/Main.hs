{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import CurrentWeather
import Data.Aeson
import Data.ByteString
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy as BSLazy
import Data.Either
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.String (fromString)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Database.Redis
import Env
import GHC.Generics
import GetFromOpenWeather
import History
import Lib
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Environment
import Web.OpenWeatherMap.Client
import Web.OpenWeatherMap.Types.Clouds
import Web.OpenWeatherMap.Types.Coord
import Web.OpenWeatherMap.Types.CurrentWeather
import Web.OpenWeatherMap.Types.Location as Loc (Location (..))
import Web.OpenWeatherMap.Types.Main
import Web.OpenWeatherMap.Types.Sys
import Web.OpenWeatherMap.Types.Weather
import Web.OpenWeatherMap.Types.Wind

type UserAPI1 =
  "current_weather" :> Capture "lon" Double :> Capture "lat" Double :> Get '[JSON] CurrentWeather
    :<|> "history" :> Capture "lon" Double :> Capture "lat" Double :> Capture "time" UTCTime :> Get '[JSON] HistoryWeather

server1 :: String -> Int -> Server UserAPI1
server1 key timeDiv =
  currentWeather key
    :<|> history timeDiv

userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 :: String -> Int -> Application
app1 key timeDiv = serve userAPI (server1 key timeDiv)

main :: IO ()
main = do
  print "server starting..."
  apiKey <- lookupEnv "openweathermap_api_key"
  settingsPath <- lookupEnv "openweathermap_settings"
  when (isNothing apiKey) (print "ERROR: set environment variable \"openweathermap_api_key\"")
  when (isNothing settingsPath) (print "ERROR: set environment variable \"openweathermap_settings\"")
  when (isJust apiKey && isJust settingsPath) $ do
    settings <- liftM (Data.Aeson.eitherDecode :: BSLazy.ByteString -> Either String Env.Settings) $ BSLazy.readFile (fromJust settingsPath)
    when (isLeft settings) (print settings)
    when (isRight settings) $ do
      let (Right s) = settings
      let env = Env (fromJust apiKey) s
      forkIO $ runReaderT runServer env
      --      forkIO $
      --        forever $ do
      --          threadDelay (updatePeriod s * 1000 * 1000 * 60)
      --          runReaderT getWeatherFromOpenWeatherServer env
      forever $ do
        threadDelay (updatePeriod s * 1000 * 1000 * 60)
        runReaderT getWeatherFromOpenWeatherServer env
      return ()
  return ()

runServer :: ReaderT Env (IO) ()
runServer = do
  port <- asks (port . env_settings)
  apiKey <- asks (env_apiKey)
  timeDiv <- asks (timeDivergense . env_settings)
  liftIO $ print $ "Running server on port " ++ show port
  liftIO $ run port (app1 apiKey timeDiv)
