module Charter (renderAndSaveLineGraph) where

import Data.Time.Clock
import Data.Time.LocalTime
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams


makeLineGraph :: [(LocalTime, Int)]
              -> String
              -> Renderable ()
makeLineGraph dataPoints graphName = toRenderable layout
  where layout = do
          layout_title      .= graphName
          layout_background .= solidFillStyle (opaque white)
          layout_foreground .= (opaque black)

          let locker = case length dataPoints of
                         0 -> []
                         _ -> [(fst $ head dataPoints, 100)]

          plot (line "" [locker])
          plot (line "Intensity" [dataPoints])

          plot $ liftEC $ do
            plot_points_values .= dataPoints
            plot_points_title  .= ""
            plot_points_style . point_radius .= 3


renderAndSaveLineGraph :: [(UTCTime, Int)]
                       -> String
                       -> String
                       -> IO (PickFn ())
renderAndSaveLineGraph dataPoints graphName outputPath = do
  timeZone <- getCurrentTimeZone

  let firsts      = map (utcToLocalTime timeZone . fst) dataPoints
      seconds     = map snd dataPoints
      dataPoints' = zip firsts seconds

  renderableToFile def outputPath (makeLineGraph dataPoints' graphName)
