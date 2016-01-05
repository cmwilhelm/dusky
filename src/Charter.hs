module Charter (renderAndSaveLineGraph) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams


makeLineGraph :: [(Int, Int)] -> String -> Renderable ()
makeLineGraph dataPoints graphName = toRenderable layout
  where layout = do
          layout_title      .= graphName
          layout_background .= solidFillStyle (opaque white)
          layout_foreground .= (opaque black)

          plot (line "Intensity" [dataPoints])


renderAndSaveLineGraph :: [(Int, Int)] -> String -> String -> IO (PickFn ())
renderAndSaveLineGraph dataPoints graphName outputPath =
  renderableToFile def outputPath (makeLineGraph dataPoints graphName)
