module Main where

import qualified Data.ByteString as B
import           Codec.Picture.Png
import           Codec.Picture.Types


data TotalRGB = TotalRGB Integer Integer Integer Int


averagedRGB :: TotalRGB
            -> (Pixel16, Pixel16, Pixel16)
averagedRGB (TotalRGB r g b n) = (avgR, avgG, avgB)
  where avgR = fromIntegral (r `div` fromIntegral n) :: Pixel16
        avgB = fromIntegral (b `div` fromIntegral n) :: Pixel16
        avgG = fromIntegral (g `div` fromIntegral n) :: Pixel16


summarizeColor :: TotalRGB
               -> Int
               -> Int
               -> PixelRGBA16
               -> TotalRGB
summarizeColor (TotalRGB rT gT bT number) x y pixel = TotalRGB newR newG newB newNumber
  where (PixelRGBA16 r g b _) = pixel
        newR                  = rT + fromIntegral r
        newG                  = gT + fromIntegral g
        newB                  = bT + fromIntegral b
        newNumber             = number + 1


findAveragePixel :: Image PixelRGBA16 -> (Pixel16, Pixel16, Pixel16)
findAveragePixel image = averagedRGB $ pixelFold summarizeColor startPoint image
  where startPoint = TotalRGB 0 0 0 0


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
  print $ findAveragePixel converted
