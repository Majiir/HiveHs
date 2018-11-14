module Hive.Coordinate (
    Coordinate,
    Direction,
    neighbors,
    neighbor,
    adjacentNeighbors,
    directions,
    walk
) where

type Coordinate = (Int, Int)
newtype Direction = Direction { index :: Int } deriving (Eq, Ord)

neighbors :: Coordinate -> [Coordinate]
neighbors c = fmap (addCoordinate c) [(-1, 0), (-1, 1), (0, 1), (1, 0), (1, -1), (0, -1)]

neighbor :: Coordinate -> Direction -> Coordinate
neighbor c d = addCoordinate c (offset d)

adjacentNeighbors :: Coordinate -> Direction -> (Coordinate, Coordinate)
adjacentNeighbors c d = (neighbor c (left d), neighbor c (right d))        

directions :: [Direction]
directions = map Direction [0..5] -- TODO: Eliminate `map`        
        
walk :: Coordinate -> Direction -> [Coordinate]
walk c d = let n = neighbor c d in n : walk n d

left :: Direction -> Direction
left = Direction . (`mod` 6) . (subtract 1) . index

right :: Direction -> Direction
right = Direction . (`mod` 6) . (+ 1) . index

addCoordinate :: Coordinate -> Coordinate -> Coordinate
addCoordinate (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

offset :: Direction -> Coordinate
offset d = [(-1, 0), (-1, 1), (0, 1), (1, 0), (1, -1), (0, -1)] !! (index d) -- TODO: Eliminate `!!`
