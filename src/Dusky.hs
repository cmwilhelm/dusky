module Dusky where

import Codec.Picture.RGBA8
import Codec.Picture.Types
import Control.Concurrent
import Control.Monad
import Charter
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import ForecastImage
import IntensityRater
import qualified LightTimes
import Locality


readInImage :: ForecastImage -> IO (Image PixelRGBA8)
readInImage = readImageRGBA8 . fileName


readInImages :: [ForecastImage] -> IO (Map.Map ForecastImage (Image PixelRGBA8))
readInImages images = do
  pixelRGBA8s <- mapM readInImage images
  return (Map.fromList $ zip images pixelRGBA8s)


rateImagesForArea :: Day
                  -> RegionShape
                  -> Map.Map ForecastImage (Image PixelRGBA8)
                  -> [ForecastImage]
                  -> [(UTCTime, Int)]
rateImagesForArea day area pRGBA8s images = mapMaybe rateImage images
  where rateImage :: ForecastImage -> Maybe (UTCTime, Int)
        rateImage image = do
          let timeValue = getUTCTimeForImage day image
          pRGBA8 <- Map.lookup image pRGBA8s

          return (timeValue, determineRegionalIntensity pRGBA8 area)


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

  sunriseTimes <- getLightTimes seattleCoords timeZone LightTimes.sunriseTimes
  sunsetTimes  <- getLightTimes seattleCoords timeZone LightTimes.sunsetTimes

  _ <- fetchSunriseImages
  _ <- fetchSunsetImages

  pixelRGBA8Map <- readInImages (sunriseImages ++ sunsetImages)
  imageMVar     <- newMVar pixelRGBA8Map
  pRGBA8Map     <- takeMVar imageMVar

  let sunriseValues    = rateImagesForArea date seattleArea pRGBA8Map sunriseImages
      sunsetValues     = rateImagesForArea date seattleArea pRGBA8Map sunsetImages
      sunriseValsLocal = convertFstsToLocalTime timeZone sunriseValues
      sunsetValsLocal  = convertFstsToLocalTime timeZone sunsetValues

  renderAndSaveLineGraph sunriseValsLocal sunriseTimes "Sunrise" "sunrise.svg"
  renderAndSaveLineGraph sunsetValsLocal sunsetTimes "Sunset" "sunset.svg"

  return ()
