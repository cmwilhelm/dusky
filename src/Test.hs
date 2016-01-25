module Test where

import Control.Concurrent
import Control.Monad
import qualified Data.Map as Map

test :: IO ()
test = do
  let myMap = Map.fromList [("asdf", 1::Int), ("fdsa", 2::Int)]
  myMVar <- newMVar myMap

  forever $ do
    value <- takeMVar myMVar
    print value
    putMVar myMVar myMap
