module Main where

import Codec.Picture.RGBA8
import Control.Monad
import Charter
import Data.Time.Calendar
import Data.Time.Clock
import FileFetcher
import IntensityRater


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


main :: IO ()
main = do
  currentTime <- getCurrentTime

  let (UTCTime date _) = currentTime

  sunriseValues <- fetchAndRateImagesForArea date seattleArea sunriseImages
  sunsetValues  <- fetchAndRateImagesForArea date seattleArea sunsetImages

  renderAndSaveLineGraph sunriseValues "Sunrise" "sunrise.svg"
  renderAndSaveLineGraph sunsetValues  "Sunset"  "sunset.svg"

  return ()
