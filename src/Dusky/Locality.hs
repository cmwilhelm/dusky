module Dusky.Locality where

type Latitude  = Float
type Longitude = Float

type Radius      = Int
type Point       = (Int, Int)
data RegionShape = Rectangle Point Point
                 | Circle Point Radius deriving (Show, Eq)


seattleCoords :: (Latitude, Longitude)
seattleCoords = (47.6097, -122.3331)


seattleArea :: RegionShape
seattleArea = shapeFromLatLng seattleCoords


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


shapeFromLatLng :: (Latitude, Longitude) -> RegionShape
shapeFromLatLng (lat, lng) = Circle point radius
  where point  = latAndLngToPoint (lat, lng)
        radius = 10


latAndLngToPoint :: (Latitude, Longitude) -> Point
latAndLngToPoint (lat, lng) = (lngToXCoord lng, latToYCoord lat)


lngToXCoord :: Float -> Int
lngToXCoord = round . getXCoord
  where getXCoord  = deriveLineFromPoints upperLeft lowerRight
        upperLeft  = (fromIntegral upperLeftLongitude,  fromIntegral upperLeftXCoord)
        lowerRight = (fromIntegral lowerRightLongitude, fromIntegral lowerRightXCoord)


latToYCoord :: Float -> Int
latToYCoord = round . getYCoord
  where getYCoord  = deriveLineFromPoints upperLeft lowerRight
        upperLeft  = (fromIntegral upperLeftLatitude,  fromIntegral upperLeftYCoord)
        lowerRight = (fromIntegral lowerRightLatitude, fromIntegral lowerRightYCoord)


deriveLineFromPoints :: (Float, Float)
                     -> (Float, Float)
                     -> Float -> Float
deriveLineFromPoints (x1, y1) (x2, y2) = \x -> slope * x + offset
  where slope  = (y1 - y2) / (x1 - x2)
        offset = y1 - slope * x1


upperLeftXCoord :: Int
upperLeftXCoord = 64

upperLeftLongitude :: Int
upperLeftLongitude = -130


upperLeftYCoord :: Int
upperLeftYCoord = 188

upperLeftLatitude :: Int
upperLeftLatitude = 54


lowerRightXCoord :: Int
lowerRightXCoord = 1334

lowerRightLongitude :: Int
lowerRightLongitude = -65


lowerRightYCoord :: Int
lowerRightYCoord = 892

lowerRightLatitude :: Int
lowerRightLatitude = 24
