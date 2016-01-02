{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Data.ByteString as B
import           Codec.Picture.Types
import           Codec.Picture.RGBA8


type Radius      = Int
type Point       = (Int, Int)
data RegionShape = Rectangle Point Point
                 | Circle Point Radius
data TotalRGB    = TotalRGB Integer Integer Integer Int


seattleArea :: RegionShape
seattleArea = Circle (214, 340) 10


takePixelAverage :: [PixelRGBA8] -> (Pixel8, Pixel8, Pixel8)
takePixelAverage = avg
                 . foldr getSum (TotalRGB 0 0 0 0)
  where avg (TotalRGB rS gS bS count) = ( pixelAverage rS count
                                        , pixelAverage gS count
                                        , pixelAverage bS count )

        pixelAverage total count      = fromIntegral (total `div` fromIntegral count)
                                      :: Pixel8

        getSum pixel (TotalRGB rS gS bS count) = TotalRGB newR newG newB (count+1)
          where (PixelRGBA8 r g b _) = pixel
                newR                  = rS + fromIntegral r
                newG                  = gS + fromIntegral g
                newB                  = bS + fromIntegral b


takeAreaAverage :: Image PixelRGBA8
                -> RegionShape
                -> (Pixel8, Pixel8, Pixel8)
takeAreaAverage image shape = takePixelAverage
                            . removeBlackPixels
                            $ selectRegion image shape


isInsideShape :: Point -> RegionShape -> Bool
isInsideShape (x,y) (Rectangle (x1,y1) (x2,y2)) = x >= x1 && x <= x2 && y >= y1 && y <= y2
isInsideShape (x,y) (Circle (xO,yO) radius) = fromIntegral radius >= distanceFromOrigin
  where distanceFromOrigin = sqrt . fromIntegral $ (dX^2 + dY^2)
        dX                 = x - xO
        dY                 = y - yO


coordsForRegion :: RegionShape -> [Point]
coordsForRegion (Rectangle (x1,y1) (x2,y2)) = [(x,y) | x <- [x1..x2], y <- [y1..y2]]
coordsForRegion (Circle (xO, yO) radius)    = filter (flip isInsideShape circle)
                                            . coordsForRegion
                                            $ Rectangle point1 point2
  where point1 = (xO - radius, yO - radius)
        point2 = (xO + radius, yO + radius)
        circle = (Circle (xO, yO) radius)


selectRegion :: (Pixel a)
             => Image a
             -> RegionShape
             -> [a]
selectRegion image shape = map (pixelAt' image) (coordsForRegion shape)
  where pixelAt' image (x, y) = pixelAt image x y


removeBlackPixels :: [PixelRGBA8] -> [PixelRGBA8]
removeBlackPixels pixels = filter notBlack pixels
  where notBlack (PixelRGBA8 r g b _) = not (r == 0 && g == 0 && b == 0)


main :: IO ()
main = do
  converted <- readImageRGBA8 "./sunrise_f9_hp.png"
  print $ takeAreaAverage converted seattleArea
