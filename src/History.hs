{-# LANGUAGE OverloadedStrings #-}

module History where

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString
import qualified Data.ByteString.Lazy as BSLazy
import Data.Maybe
import Data.String (fromString)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Database.Redis
import RedisCommands
import Servant
import Web.OpenWeatherMap.Types.CurrentWeather

type HistoryWeather = CurrentWeather

type Lon = Double

type Lat = Double

history :: Int -> Lon -> Lat -> UTCTime -> Handler HistoryWeather
history timeDiv lon lat t = do
  res <- liftIO $ getFromRedis timeDiv lon lat t
  case res of
    Just x -> return x
    _ -> throwError (err500 {errBody = "No history data"})

getFromRedis :: Int -> Lon -> Lat -> UTCTime -> IO (Maybe HistoryWeather)
getFromRedis timeDiv lon lat time = do
  loc <- searchLocation lon lat 50
  case loc of
    Nothing -> return Nothing
    Just x -> searchLocationHistory timeDiv (fromJust loc) time

searchLocationHistory :: Int -> ByteString -> UTCTime -> IO (Maybe HistoryWeather)
searchLocationHistory timeDiv locName time = do
  conn <- checkedConnect defaultConnectInfo
  runRedis conn $ do
    res <- zrangebyscore locName (fromIntegral $ time' - 60 * timeDiv) (fromIntegral $ time' + 60 * timeDiv)
    case res of
      Right (x : xs) -> do
        return . Data.Aeson.decode . BSLazy.fromStrict $ Data.ByteString.drop 10 x
      _ -> do
        return Nothing
  where
    time' = fromIntegral . floor . utcTimeToPOSIXSeconds $ time

searchLocation :: Double -> Double -> Double -> IO (Maybe ByteString)
searchLocation lon lat rad = do
  conn <- checkedConnect defaultConnectInfo
  runRedis conn $ do
    let [lon', lat', rad'] = Prelude.map (fromString . show) [lon, lat, rad]
    Left res <- georadius "locations" lon' lat' rad'
    return $ case res of
      MultiBulk (Just []) -> Nothing
      MultiBulk (Just ((Bulk x) : xs)) -> x
      Database.Redis.Error _ -> Nothing
