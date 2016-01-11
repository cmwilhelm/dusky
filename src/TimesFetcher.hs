{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TimesFetcher where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens (key)
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Maybe (fromJust)
import Data.Text as T
import Data.Time.Clock
import Data.Time.ISO8601 (parseISO8601)
import Network.Wreq


data LightTimes = LightTimes { ltSunrise               :: UTCTime
                             , ltSunset                :: UTCTime
                             , ltCivilTwilightBegin    :: UTCTime
                             , ltCivilTwilightEnd      :: UTCTime
                             , ltNauticalTwilightBegin :: UTCTime
                             , ltNauticalTwilightEnd   :: UTCTime
                             , ltAstroTwilightBegin    :: UTCTime
                             , ltAstroTwilightEnd      :: UTCTime
                             } deriving (Show, Eq)


parseAndConvertKey :: Object -> Text -> Parser UTCTime
parseAndConvertKey dict key = do
  rawValue <- dict .: key

  let value = parseISO8601
            . T.unpack
            $ rawValue

  result <- case value of
    Just t -> return t
    _      -> fail $ (T.unpack key) ++ " was not recognized as ISO8601 time"

  return result


instance FromJSON LightTimes where
  parseJSON = withObject "lightTimes" $ \dict -> do
    let extract = parseAndConvertKey dict

    ltSunrise               <- extract "sunrise"
    ltSunset                <- extract "sunset"
    ltCivilTwilightBegin    <- extract "civil_twilight_begin"
    ltCivilTwilightEnd      <- extract "civil_twilight_end"
    ltNauticalTwilightBegin <- extract "nautical_twilight_begin"
    ltNauticalTwilightEnd   <- extract "nautical_twilight_end"
    ltAstroTwilightBegin    <- extract "astronomical_twilight_begin"
    ltAstroTwilightEnd      <- extract "astronomical_twilight_end"

    return LightTimes{..}


getTimesForToday :: Float -> Float -> IO (Maybe LightTimes)
getTimesForToday lat lng = do
  let baseUrl = "http://api.sunrise-sunset.org/json"
  let opts    = defaults & param "formatted" .~ ["0"]
                         & param "lat" .~ [T.pack $ show lat]
                         & param "lng" .~ [T.pack $ show lng]
  r <- getWith opts baseUrl

  let (Just result) = r ^? responseBody . key "results"

  return (parseMaybe parseJSON result)
