{-# LANGUAGE DeriveGeneric #-}

module Env where

import Data.Aeson
import GHC.Generics

instance FromJSON Settings

instance FromJSON Lctn

data Settings = Settings
  { port :: Int,
    locations :: [Lctn],
    updatePeriod :: Int, -- min
    locationDivergense :: Int, -- km
    timeDivergense :: Int -- min
  }
  deriving (Generic, Show)

data Env = Env
  { env_apiKey :: String,
    env_settings :: Settings
  }
  deriving (Show)

data Lctn = Lctn -- Location
  { lat :: Double,
    lon :: Double
  }
  deriving (Generic, Show)
