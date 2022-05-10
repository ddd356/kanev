{-# LANGUAGE OverloadedStrings #-}

module RedisCommands where

import Data.ByteString
import Database.Redis

georadius :: (RedisCtx m f) => ByteString -> ByteString -> ByteString -> ByteString -> m (f Status)
georadius key lon lat radius = sendRequest ["georadius", key, lon, lat, radius, "km"]

geoadd :: (RedisCtx m f) => ByteString -> ByteString -> ByteString -> ByteString -> m (f Status)
geoadd key lon lat member = sendRequest ["geoadd", key, lon, lat, member]
