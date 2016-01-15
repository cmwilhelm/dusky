module ForecastImage where

import Control.Monad
import qualified Data.ByteString as B
import Data.Time.Calendar
import Data.Time.Clock
import Network.HTTP
import Network.URI (parseURI)


data ForecastType  = Sunrise | Sunset deriving (Show, Eq)
data ForecastImage = ForecastImage { forecastType :: ForecastType
                                   , imageUrl     :: String
                                   , fileName     :: String
                                   , time         :: Int
                                   } deriving (Show, Eq)


baseUrl :: String
baseUrl = "http://sunsetwx.com/"


sunriseImages :: [ForecastImage]
sunriseImages = map makeImage [8..23]
  where buildUrl zTime      =  baseUrl
                            ++ "sunrise/sunrise_f"
                            ++ show (zTime - 5)
                            ++ ".png"

        buildFileName zTime =  "sunrise_f"
                            ++ show (zTime - 5)
                            ++ ".png"

        makeImage zTime     = ForecastImage { forecastType = Sunrise
                                            , imageUrl     = buildUrl zTime
                                            , fileName     = buildFileName zTime
                                            , time         = zTime }


sunsetImages :: [ForecastImage]
sunsetImages = map makeImage [20..36]
  where buildUrl zTime      =  baseUrl
                            ++ "sunset/sunset_f"
                            ++ show (zTime - 18)
                            ++ ".png"

        buildFileName zTime =  "sunset_f"
                            ++ show (zTime - 18)
                            ++ ".png"

        makeImage zTime     = ForecastImage { forecastType = Sunset
                                            , imageUrl     = buildUrl zTime
                                            , fileName     = buildFileName zTime
                                            , time         = zTime }


get :: String -> IO B.ByteString
get url = let uri = case parseURI url of
                      Nothing -> error $ "Invalid URI: " ++ url
                      Just u -> u in
            simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody


fetchImage :: ForecastImage -> IO ()
fetchImage (ForecastImage _ url fName _) = do
  png <- get url
  B.writeFile fName png


fetchSunriseImages :: IO ()
fetchSunriseImages = forM_ sunriseImages fetchImage


fetchSunsetImages :: IO ()
fetchSunsetImages = forM_ sunsetImages fetchImage


getUTCTimeForImage :: Day -> ForecastImage -> UTCTime
getUTCTimeForImage day image = addUTCTime diffTime (UTCTime day 0)
  where diffTime :: NominalDiffTime
        diffTime = fromIntegral (time image) * 60 * 60
