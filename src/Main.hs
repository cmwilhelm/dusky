module Main where

import Codec.Picture.RGBA8
import Control.Monad
import Charter
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import ForecastImage
import IntensityRater
import qualified LightTimes
import Locality


seattleCoords :: (Latitude, Longitude)
seattleCoords = (47.6097, (-122.3331))

seattleArea :: RegionShape
seattleArea = Circle (214, 340) 10


fetchAndRateImageForArea :: Day
                         -> RegionShape
                         -> ForecastImage
                         -> IO (UTCTime, Int)
fetchAndRateImageForArea day area image = do
  _         <- fetchImage image
  converted <- readImageRGBA8 (fileName image)

  let timeValue = getUTCTimeForImage day image
      intensity = determineRegionalIntensity converted seattleArea

  return (timeValue, intensity)


fetchAndRateImagesForArea :: Day
                          -> RegionShape
                          -> [ForecastImage]
                          -> IO ([(UTCTime, Int)])
fetchAndRateImagesForArea day area images =
  mapM (fetchAndRateImageForArea day area) images


convertFstsToLocalTime :: TimeZone
                       -> [(UTCTime, a)]
                       -> [(LocalTime, a)]
convertFstsToLocalTime tz timeTuples = map convert timeTuples
  where convert (utcTime, second) = (utcToLocalTime tz utcTime, second)


getLightTimes :: (Latitude, Longitude)
              -> TimeZone
              -> ( LightTimes.LightTimes -> [(UTCTime, String)] )
              -> IO [(LocalTime, String)]
getLightTimes (lat, lng) tz timesOfInterest = do
  maybeLightTimes <- LightTimes.getTimesForToday lat lng

  let results = case maybeLightTimes of
                  Just lightTimes -> (convertFstsToLocalTime tz) (timesOfInterest lightTimes)
                  Nothing         -> []

  return results


main :: IO ()
main = do
  timeZone         <- getCurrentTimeZone
  (UTCTime date _) <- getCurrentTime

  let coords = (0, 0)

  sunriseTimes   <- getLightTimes seattleCoords timeZone LightTimes.sunriseTimes
  sunsetTimes    <- getLightTimes seattleCoords timeZone LightTimes.sunsetTimes

  sunriseValues  <- fetchAndRateImagesForArea date seattleArea sunriseImages
  sunsetValues   <- fetchAndRateImagesForArea date seattleArea sunsetImages

  let sunriseValuesLocal = convertFstsToLocalTime timeZone sunriseValues
      sunsetValuesLocal  = convertFstsToLocalTime timeZone sunsetValues

  renderAndSaveLineGraph sunriseValuesLocal sunriseTimes "Sunrise" "sunrise.svg"
  renderAndSaveLineGraph sunsetValuesLocal sunsetTimes "Sunset" "sunset.svg"

  return ()
