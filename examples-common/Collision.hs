module Collision where

import           Control.Applicative
import           Types
import           SDL.Vect                hiding ( trace )

data Shape = Sphere Position Radius
             | AABB Position Bounds

data Side = TopSide | BottomSide | LeftSide | RightSide

data ToShape a = ToShape
  { broadphaseShape :: Shape
  , narrowphaseShape :: Shape
  }

shapeColliding :: Shape -> Shape -> Bool
shapeColliding (Sphere p r) (Sphere p' r') = sphereColliding p r p' r'
shapeColliding (Sphere p r) a = distance p (closestPtPointAABB a p) <= (r * r)
shapeColliding a (Sphere p r) = distance p (closestPtPointAABB a p) <= (r * r)
shapeColliding _ _ = undefined

sphereColliding :: Position -> Radius -> Position -> Radius -> Bool
sphereColliding p r p' r' = distance p p' < r + r'

sphereAABBColliding :: Position -> Radius -> Position -> Bounds -> Bool
sphereAABBColliding _ _ _ _ = undefined

broadphase :: ToShape a -> ToShape b -> Bool
broadphase a b = shapeColliding (broadphaseShape a) (broadphaseShape b)

narrowphase :: ToShape a -> ToShape b -> Bool
narrowphase a b = shapeColliding (narrowphaseShape a) (narrowphaseShape b)

boundsColliding :: Int -> Int -> Int -> Int -> Shape -> Maybe Side
boundsColliding xMin xMax yMin yMax (Sphere p r) =
  choice (left <= xMin) LeftSide
    <|> choice (top >= yMax)    TopSide
    <|> choice (right >= xMax)  RightSide
    <|> choice (bottom <= yMin) BottomSide
 where
  left = case p - V2 r 0 of
    V2 x _ -> round x
  right = case p + V2 r 0 of
    V2 x _ -> round x
  top = case p + V2 0 r of
    V2 _ y -> round y
  bottom = case p - V2 0 r of
    V2 _ y -> round y

boundsColliding _ _ _ _ (AABB _ _) = undefined

-- if the bool is true, returns Just a, else returns Nothing
choice :: Bool -> a -> Maybe a
choice bool a = if bool then Just a else Nothing

minX :: Shape -> Double
minX (AABB pos _) = case pos of
  V2 x _ -> x
minX _ = undefined

maxX :: Shape -> Double
maxX (AABB pos bs) = case pos ^+^ bs of
  V2 x _ -> x
maxX _ = undefined

minY :: Shape -> Double
minY (AABB pos _) = case pos of
  V2 _ y -> y
minY _ = undefined

maxY :: Shape -> Double
maxY (AABB pos bs) = case pos ^+^ bs of
  V2 _ y -> y
maxY _ = undefined

-- Returns a point on an AABB that is closest to another point
-- Implementation, see p.130 [https://doi.org/10.1201/b14581]
closestPtPointAABB :: Shape -> Position -> Position
closestPtPointAABB aabb pos = pos'
 where
  posX = case pos of
    V2 x' _ -> x'
  posY = case pos of
    V2 _ y' -> y'
  aabbMinX = minX aabb
  aabbMaxX = maxX aabb
  aabbMinY = minY aabb
  aabbMaxY = maxY aabb
  x        = min (max posX aabbMinX) aabbMaxX
  y        = min (max posY aabbMinY) aabbMaxY
  pos'     = V2 x y

