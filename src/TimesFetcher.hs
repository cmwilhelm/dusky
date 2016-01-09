{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TimesFetcher where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens (key)
import Data.Aeson.Types (parseMaybe)
import Data.Text as T
import Network.Wreq


data LightTimes = LightTimes { ltSunrise               :: String
                             , ltSunset                :: String
                             , ltCivilTwilightBegin    :: String
                             , ltCivilTwilightEnd      :: String
                             , ltNauticalTwilightBegin :: String
                             , ltNauticalTwilightEnd   :: String
                             , ltAstroTwilightBegin    :: String
                             , ltAstroTwilightEnd      :: String
                             } deriving (Show, Eq)


instance FromJSON LightTimes where
  parseJSON = withObject "lightTimes" $ \dict -> do
    ltSunrise               <- dict .: "sunrise"
    ltSunset                <- dict .: "sunset"
    ltCivilTwilightBegin    <- dict .: "civil_twilight_begin"
    ltCivilTwilightEnd      <- dict .: "civil_twilight_end"
    ltNauticalTwilightBegin <- dict .: "nautical_twilight_begin"
    ltNauticalTwilightEnd   <- dict .: "nautical_twilight_end"
    ltAstroTwilightBegin    <- dict .: "astronomical_twilight_begin"
    ltAstroTwilightEnd      <- dict .: "astronomical_twilight_end"

    return LightTimes{..}


getTimesForToday :: Float -> Float -> IO (Maybe LightTimes)
getTimesForToday lat lng = do
  let baseUrl = "http://api.sunrise-sunset.org/json"
  let opts    = defaults & param "formatted" .~ ["1"]
                         & param "lat" .~ [T.pack $ show lat]
                         & param "lng" .~ [T.pack $ show lng]
  r <- getWith opts baseUrl

  let (Just result) = r ^? responseBody . key "results"

  return (parseMaybe parseJSON result)
