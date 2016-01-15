module Locality where

type Latitude  = Float
type Longitude = Float

type Radius      = Int
type Point       = (Int, Int)
data RegionShape = Rectangle Point Point
                 | Circle Point Radius


isInsideShape :: Point -> RegionShape -> Bool
isInsideShape (x,y) (Rectangle (x1,y1) (x2,y2)) = x >= x1 && x <= x2 && y >= y1 && y <= y2
isInsideShape (x,y) (Circle (xO,yO) radius) = fromIntegral radius >= distanceFromOrigin
  where distanceFromOrigin = sqrt . fromIntegral $ (dX^2 + dY^2)
        dX                 = x - xO
        dY                 = y - yO


coordsForRegion :: RegionShape -> [Point]
coordsForRegion (Rectangle (x1,y1) (x2,y2)) = [(x,y) | x <- [x1..x2], y <- [y1..y2]]
coordsForRegion c@(Circle (xO, yO) radius)  = filter (flip isInsideShape c)
                                            . coordsForRegion
                                            $ Rectangle point1 point2
  where point1 = (xO - radius, yO - radius)
        point2 = (xO + radius, yO + radius)
