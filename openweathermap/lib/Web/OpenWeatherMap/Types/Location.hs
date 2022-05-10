{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Web.OpenWeatherMap.Types.Location
  ( Location (..),
  )
where

import Data.Proxy (Proxy (..))
import Data.Text.Encoding (encodeUtf8)
import Servant.API ((:>))
import Servant.Client (Client, HasClient, clientWithRoute, hoistClientMonad)
import Servant.Client.Core.Request (appendToQueryString)
import Web.HttpApiData (toQueryParam)

-- | Various way to specify location.
data Location
  = -- | City name.
    Name String
  | -- | Geographic coordinates: latitude and longitude.
    Coord Double Double

instance HasClient m api => HasClient m (Location :> api) where
  type Client m (Location :> api) = Location -> Client m api
  clientWithRoute pm Proxy req loc =
    clientWithRoute pm (Proxy :: Proxy api) (addParams loc req)
    where
      addParams (Name q) = appendToQueryString "q" (Just . encodeUtf8 $ toQueryParam q)
      addParams (Coord lat lon) =
        appendToQueryString "lat" (Just . encodeUtf8 $ toQueryParam lat)
          . appendToQueryString "lon" (Just . encodeUtf8 $ toQueryParam lon)
  hoistClientMonad pm _ f cl =
    \a -> hoistClientMonad pm (Proxy :: Proxy api) f (cl a)
