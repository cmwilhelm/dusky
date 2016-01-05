module Charter (renderAndSaveLineGraph) where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams


makeLineGraph :: [(Int, Int)] -> String -> Renderable ()
makeLineGraph dataPoints graphName = toRenderable layout
  where layout = do
          layout_title      .= graphName
          layout_background .= solidFillStyle (opaque white)
          layout_foreground .= (opaque black)

          let locker = if length dataPoints > 0
                         then (fst $ head dataPoints, 100)
                         else (0, 100)

          plot (line "Intensity" [dataPoints])
          plot (line "" [[locker]])

          plot $ liftEC $ do
            plot_points_values .= dataPoints
            plot_points_title  .= ""
            plot_points_style . point_radius .= 3


renderAndSaveLineGraph :: [(Int, Int)] -> String -> String -> IO (PickFn ())
renderAndSaveLineGraph dataPoints graphName outputPath =
  renderableToFile def outputPath (makeLineGraph dataPoints graphName)
