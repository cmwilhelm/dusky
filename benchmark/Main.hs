module Main
  ( main
  ) where

import Criterion.Main (defaultMain, bgroup, bench, whnfIO)
import Dusky (getLightTimes)
import qualified LightTimes
import Data.Time.LocalTime

main = do
  timeZone         <- getCurrentTimeZone
  defaultMain [
    bgroup "getLightTimes"
      [
        bench "nyc" $ whnfIO $ getLightTimes (40.7127, 74.0059) timeZone LightTimes.sunriseTimes
      ]
    ]
