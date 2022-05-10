{-# LANGUAGE OverloadedStrings #-}

module CurrentWeather where

import Control.Monad.IO.Class
import Servant
import Web.OpenWeatherMap.Client
import Web.OpenWeatherMap.Types.CurrentWeather
import Web.OpenWeatherMap.Types.Location as Loc (Location (..))

type Lon = Double

type Lat = Double

currentWeather :: String -> Lon -> Lat -> Handler CurrentWeather
currentWeather apiKey lon lat = do
  res <- liftIO $ getWeather apiKey location
  case res of
    Right x -> return x
    _ -> throwError (err500 {errBody = "Can't get current weather"})
  where
    location = Loc.Coord lat lon
