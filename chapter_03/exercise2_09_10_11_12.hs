import Data.List (minimumBy, sortBy)

data Direction = LeftTurn | RightTurn | Straight deriving (Show)

data Point = Point Double Double

calculateTurn :: Point -> Point -> Point -> Direction
calculateTurn (Point x1 y1) (Point x2 y2) (Point x3 y3)
  | cross == 0 = Straight
  | cross > 0 = LeftTurn
  | otherwise = RightTurn
  where
    cross = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

calculateTurns :: [Point] -> [Direction]
calculateTurns [] = []
calculateTurns [p1, p2] = []
calculateTurns (p1 : p2 : p3 : ps) = calculateTurn p1 p2 p3 : calculateTurns (p2 : (p3 : ps))

compareByYLeftmost :: Point -> Point -> Ordering
compareByYLeftmost (Point x1 y1) (Point x2 y2)
  | y1 < y2 = LT
  | y1 > y2 = GT
  | y1 == y2 && x1 < x2 = LT
  | y1 == y2 && x1 > x2 = GT
  | otherwise = EQ

lowestYLeftmost :: [Point] -> Point
lowestYLeftmost = minimumBy compareByYLeftmost

distanceSquared :: Point -> Point -> Double
distanceSquared (Point x1 y1) (Point x2 y2) = (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)

compareByTurnAndDistanceSquared :: Direction -> Double -> Double -> Ordering
compareByTurnAndDistanceSquared LeftTurn _ _ = LT
compareByTurnAndDistanceSquared RightTurn _ _ = GT
compareByTurnAndDistanceSquared Straight dist1 dist2
  | dist1 < dist2 = LT
  | dist1 > dist2 = GT
  | dist1 == dist2 = EQ

compareByPolarAngleAndDistance :: Point -> Point -> Point -> Ordering
compareByPolarAngleAndDistance p0 p1 p2 = compareByTurnAndDistanceSquared turn d1 d2
  where
    turn = calculateTurn p0 p1 p2
    d1 = distanceSquared p0 p1
    d2 = distanceSquared p0 p2

sortedByPolarAngle :: Point -> [Point] -> [Point]
sortedByPolarAngle p0 = sortBy (compareByPolarAngleAndDistance p0)

grahamScanEliminate :: [Point] -> [Point] -> [Point]
grahamScanEliminate (t1 : traversed) (p1 : p2 : p3 : ps) =
  case calculateTurn p1 p2 p3 of
    LeftTurn -> grahamScanEliminate (p1 : t1 : traversed) (p2 : p3 : ps)
    RightTurn -> grahamScanEliminate traversed (t1 : p1 : p3 : ps)

grahamScan :: [Point] -> [Point]
grahamScan ps =
  let p0 = lowestYLeftmost ps
      qs = sortedByPolarAngle p0 ps
   in grahamScanEliminate [] qs
