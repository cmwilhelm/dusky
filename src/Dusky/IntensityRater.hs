module Dusky.IntensityRater
  ( determineRegionalIntensity
  ) where

import Codec.Picture.Types
import Data.List
import qualified Data.Map.Strict as Map

import Dusky.Locality


toPercent :: Float -> Int
toPercent = round . (* 100)


takeWeightedAverage :: [(Float, Int)] -> Float
takeWeightedAverage values = foldr weightedSum 0 values / fromIntegral totalCount
  where weightedSum (v, count) acc = acc + (fromIntegral count * v)
        totalCount                 = foldr ((+) . snd) 0 values


findBestFits :: [PixelRGBA8]
             -> Map.Map PixelRGBA8 Int
             -> Map.Map Float Int
findBestFits intensities pixelFreqs = Map.foldrWithKey foldFn (Map.fromList []) pixelFreqs
  where foldFn key value acc = Map.insert (bestFitIntensity key intensities) value acc


bestFitIntensity :: PixelRGBA8 -> [PixelRGBA8] -> Float
bestFitIntensity _ []              = 0
bestFitIntensity pixel intensities = asProportion
                                   . snd
                                   . minimum
                                   $ zip (map (takeDelta pixel) intensities) [1..]

  where asProportion index = 1 - (fromIntegral index / fromIntegral (length intensities))

        takeDelta (PixelRGBA8 r1 g1 b1 _) (PixelRGBA8 r2 g2 b2 _) =
          abs ( fromIntegral r1 - fromIntegral r2 ) +
          abs ( fromIntegral g1 - fromIntegral g2 ) +
          abs ( fromIntegral b1 - fromIntegral b2 )


getIntensities :: Image PixelRGBA8 -> [PixelRGBA8]
getIntensities = nub . flip selectRegion intensityPalette
  where intensityPalette = Rectangle (1340, 143) (1340, 935)


countFrequencies :: [PixelRGBA8] -> Map.Map PixelRGBA8 Int
countFrequencies pixels = Map.fromListWith (+) (map (\p -> (p, 1)) pixels)


removeBlackPixels :: [PixelRGBA8] -> [PixelRGBA8]
removeBlackPixels = filter notBlack
  where notBlack (PixelRGBA8 r g b _) = not (r == 0 && g == 0 && b == 0)


selectRegion :: (Pixel a)
             => Image a
             -> RegionShape
             -> [a]
selectRegion image shape = map (pixelAt' image) (coordsForRegion shape)
  where pixelAt' image' (x, y) = pixelAt image' x y


determineRegionalIntensity :: Image PixelRGBA8 -> RegionShape -> Int
determineRegionalIntensity image shape = toPercent
                                       . takeWeightedAverage
                                       . Map.toList
                                       . findBestFits (getIntensities image)
                                       . countFrequencies
                                       . removeBlackPixels
                                       $ selectRegion image shape
