module Main where

import qualified Data.ByteString as B
import           Codec.Picture.Png
import           Codec.Picture.Types


type Radius      = Int
type Point       = (Int, Int)
data RegionShape = Rectangle Point Point
                 | Circle Point Radius
data TotalRGB    = TotalRGB Integer Integer Integer Int


seattleArea :: RegionShape
seattleArea = Circle (214, 340) 10


takePixelAverage :: [PixelRGBA16] -> (Pixel16, Pixel16, Pixel16)
takePixelAverage = avg
                 . foldr getSum (TotalRGB 0 0 0 0)
  where avg (TotalRGB rS gS bS count) = ( pixelAverage rS count
                                        , pixelAverage gS count
                                        , pixelAverage bS count )

        pixelAverage total count      = fromIntegral (total `div` fromIntegral count)
                                      :: Pixel16

        getSum pixel (TotalRGB rS gS bS count) = TotalRGB newR newG newB (count+1)
          where (PixelRGBA16 r g b _) = pixel
                newR                  = rS + fromIntegral r
                newG                  = gS + fromIntegral g
                newB                  = bS + fromIntegral b


takeAreaAverage :: Image PixelRGBA16
                -> RegionShape
                -> (Pixel16, Pixel16, Pixel16)
takeAreaAverage image shape = takePixelAverage
                            . removeBlackPixels
                            $ selectRegion image shape


isInsideShape :: Point -> RegionShape -> Bool
isInsideShape (x,y) (Rectangle (x1,y1) (x2,y2)) = x >= x1 && x <= x2 && y >= y1 && y <= y2
isInsideShape (x,y) (Circle (xO,yO) radius) = fromIntegral radius >= distanceFromOrigin
  where distanceFromOrigin = sqrt . fromIntegral $ (dX^2 + dY^2)
        dX                 = x - xO
        dY                 = y - yO


selectRegion :: Image PixelRGBA16
             -> RegionShape
             -> [PixelRGBA16]
selectRegion image shape = pixelFold (specifiedRegionOnly shape) [] image
  where specifiedRegionOnly :: (Pixel a)
                            => RegionShape
                            -> [a]
                            -> Int
                            -> Int
                            -> a
                            -> [a]
        specifiedRegionOnly shape validPixels x y pixel
          | isInsideShape (x,y) shape = pixel : validPixels
          | otherwise                 = validPixels


removeBlackPixels :: [PixelRGBA16] -> [PixelRGBA16]
removeBlackPixels pixels = filter notBlack pixels
  where notBlack (PixelRGBA16 r g b _) = not (r == 0 && g == 0 && b == 0)


promotePng :: DynamicImage -> Image PixelRGBA16
promotePng (ImageY8 image)     = promoteImage (promoteImage image :: (Image PixelRGB8)) :: (Image PixelRGBA16)
promotePng (ImageY16 image)    = promoteImage image :: (Image PixelRGBA16)
promotePng (ImageYA8 image)    = promoteImage (promoteImage image :: (Image PixelRGBA8)) :: (Image PixelRGBA16)
promotePng (ImageYA16 image)   = promoteImage image :: (Image PixelRGBA16)
promotePng (ImageRGB8 image)   = promoteImage image :: (Image PixelRGBA16)
promotePng (ImageRGB16 image)  = promoteImage image :: (Image PixelRGBA16)
promotePng (ImageRGBA8 image)  = promoteImage image :: (Image PixelRGBA16)
promotePng (ImageRGBA16 image) = image


readPngAsRGBA16 :: String -> IO (Image PixelRGBA16)
readPngAsRGBA16 filePath = do
  fileContents <- B.readFile filePath
  let (Right dynamicImg) = decodePng fileContents
  return (promotePng dynamicImg)


main :: IO ()
main = do
  converted <- readPngAsRGBA16 "./sunrise_f9_hp.png"
  print $ takeAreaAverage converted seattleArea
