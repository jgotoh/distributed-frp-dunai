module Collision where

import           Control.Applicative
import           Types
import           SDL.Vect

data Shape = Sphere Position Radius
             | AABB Position Bounds

data Side = TopSide | BottomSide | LeftSide | RightSide

data ToShape a = ToShape
  { broadphaseShape :: Shape
  , narrowphaseShape :: Shape
  }
-- TODO replace return values with maybe position to get access to monad maybe functions

shapeColliding :: Shape -> Shape -> Bool
shapeColliding (Sphere p r) (Sphere p' r') = sphereColliding p r p' r'
shapeColliding (Sphere p r) (AABB   p' b ) = sphereAABBColliding p r p' b
shapeColliding (AABB   p b) (Sphere p' r ) = sphereAABBColliding p' r p b
shapeColliding _            _              = undefined

sphereColliding :: Position -> Radius -> Position -> Radius -> Bool
sphereColliding p r p' r' = distance p p' < r + r'

sphereAABBColliding :: Position -> Radius -> Position -> Bounds -> Bool
sphereAABBColliding _ _ _ _ = True

broadphase :: ToShape a -> ToShape b -> Bool
broadphase a b = shapeColliding (broadphaseShape a) (broadphaseShape b)

narrowphase :: ToShape a -> ToShape b -> Bool
narrowphase a b = shapeColliding (narrowphaseShape a) (narrowphaseShape b)

boundsColliding :: Int -> Int -> Int -> Int -> Shape -> Maybe Side
boundsColliding xMin xMax yMin yMax (Sphere p r) =
  (choice (left < xMin) LeftSide)
    <|> (choice (top > yMax) TopSide)
    <|> (choice (right > xMax) RightSide)
    <|> (choice (bottom < yMin) BottomSide)
 where
  left = case p - (V2 (-r) 0) of
    V2 x _ -> round x
  right = case p + (V2 r 0) of
    V2 x _ -> round x
  top = case p + (V2 0 r) of
    V2 _ y -> round y
  bottom = case p + (V2 0 (-r)) of
    V2 _ y -> round y

-- if the bool is true, returns Just a, else returns Nothing
choice :: Bool -> a -> Maybe a
choice bool a = if bool then (Just a) else Nothing

