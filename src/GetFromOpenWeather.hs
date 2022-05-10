{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module GetFromOpenWeather where

import Control.Monad.Reader
import Data.Aeson
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy as BSLazy
import Data.Either
import Data.List
import Data.String (fromString)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Database.Redis
import Env
import RedisCommands
import Web.OpenWeatherMap.Client
import Web.OpenWeatherMap.Types.Clouds
import Web.OpenWeatherMap.Types.Coord
import Web.OpenWeatherMap.Types.CurrentWeather
import Web.OpenWeatherMap.Types.Location as Loc (Location (..))
import Web.OpenWeatherMap.Types.Main
import Web.OpenWeatherMap.Types.Sys
import Web.OpenWeatherMap.Types.Weather
import Web.OpenWeatherMap.Types.Wind

instance ToJSON Sys

instance ToJSON Coord

instance ToJSON Weather

instance ToJSON Clouds

instance ToJSON Main

instance ToJSON Wind

instance ToJSON CurrentWeather

getWeatherFromOpenWeatherServer :: ReaderT Env (IO) ()
getWeatherFromOpenWeatherServer = do
  time <- liftIO $ getCurrentTime
  let timeIntegral = floor . utcTimeToPOSIXSeconds $ time
  let timeDouble = fromIntegral timeIntegral
  l <- asks (locations . env_settings)
  apiKey <- asks (env_apiKey)
  either_weather <-
    liftIO . sequence $
      let f = \(Lctn lat lon) -> getWeather apiKey (location lat lon)
          location x y = Loc.Coord x y
       in Data.List.map f l
  let weather = Data.List.filter (\(x, _, _) -> isRight x) $ Data.List.zip3 either_weather l (Data.List.map (\(Lctn lat lon) -> show lon ++ "," ++ show lat) l)
  conn <- liftIO $ checkedConnect defaultConnectInfo
  liftIO $ sequence_ $ Data.List.map (\(Right member, _, key) -> runRedis conn $ zadd (fromString key) [(timeDouble, fromString $ show timeIntegral ++ (Data.ByteString.Char8.unpack . BSLazy.toStrict . encode $ member))]) weather
  liftIO $
    sequence_ $
      Data.List.map
        ( \(_, (Lctn lat lon), key) ->
            runRedis conn $
              geoadd "locations" (fromString $ show lon) (fromString $ show lat) (fromString $ show lon ++ "," ++ show lat)
        )
        weather
  return ()
