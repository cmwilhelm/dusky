module Main where

import Codec.Picture.RGBA8
import Control.Monad
import Charter
import FileFetcher
import IntensityRater


seattleArea :: RegionShape
seattleArea = Circle (214, 340) 10


fetchAndRateImageForArea :: RegionShape -> ForecastImage -> IO (Int, Int)
fetchAndRateImageForArea area image = do
  _         <- fetchImage image
  converted <- readImageRGBA8 (fileName image)

  let timeValue = time image
      intensity = determineRegionalIntensity converted seattleArea

  return (timeValue, intensity)


fetchAndRateImagesForArea :: RegionShape -> [ForecastImage] -> IO ([(Int, Int)])
fetchAndRateImagesForArea area images = mapM (fetchAndRateImageForArea area) images


main :: IO ()
main = do
  sunriseValues <- fetchAndRateImagesForArea seattleArea sunriseImages
  sunsetValues  <- fetchAndRateImagesForArea seattleArea sunsetImages

  renderAndSaveLineGraph sunriseValues "Sunrise" "sunrise.svg"
  renderAndSaveLineGraph sunsetValues  "Sunset"  "sunset.svg"

  return ()
