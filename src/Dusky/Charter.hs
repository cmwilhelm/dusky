module Dusky.Charter (renderAndSaveLineGraph) where

import Control.Monad
import Data.Time.LocalTime
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams


makeLineGraph :: [(LocalTime, Int)]
              -> [(LocalTime, String)]
              -> String
              -> Renderable ()
makeLineGraph dataPoints verticals graphName = toRenderable layout
  where layout = do
          layout_title      .= graphName
          layout_background .= solidFillStyle (opaque white)
          layout_foreground .= (opaque black)

          forM_ verticals (\(lT, label) -> plot (line label [ [(lT, 0), (lT, 100)] ]))

          plot (line "Quality of Conditions" [dataPoints])

          plot $ liftEC $ do
            plot_points_values .= dataPoints
            plot_points_title  .= ""
            plot_points_style . point_radius .= 3


renderAndSaveLineGraph :: [(LocalTime, Int)]
                       -> [(LocalTime, String)]
                       -> String
                       -> String
                       -> IO (PickFn ())
renderAndSaveLineGraph dataPoints verticals graphName outputPath = do
  renderableToFile def outputPath (makeLineGraph dataPoints verticals graphName)
