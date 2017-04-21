module Xinyue where

import DataTypes
import Rachel
import qualified Data.Vector as V
import qualified Data.Matrix as M

data Vector3 a = Vector3 a a a

minus :: Vec3 -> Vec3 -> Vec3
minus (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)

plus :: Vec3 -> Vec3 -> Vec3
plus (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

multScaler :: Vec3 -> Double -> Vec3
multScaler (Vec3 x y z) k = Vec3 (k * x) (k * y) (k * z)

normalize :: Vec3 -> Vec3
normalize (Vec3 x1 y1 z1) =
  let invlen = 1.0 / (sqrt ((x1 * x1) + (y1 * y1) + (z1 * z1))) in
    Vec3 (x1 * invlen) (y1 * invlen) (z1 * invlen)

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  Vec3
  ((y1 * z2) - (z1 * y2))
  ((z1 * x2) - (x1 * z2))
  ((x1 * y2) - (y1 * x2))

-- Calculate the color of a point on an object based on the Phong reflection model
phong :: Ray -> Point -> Object -> [Object] -> [Light] -> Color
phong = error "Not Implemented"

checkIntersect :: Ray -> [Object] -> Maybe (Object, Double)
checkIntersect _ []             = Nothing
checkIntersect ray (obj : objs) = let t = getIntersect ray obj in
  case checkIntersect ray objs of
    Nothing                         -> if t < 0 then Nothing
                                       else Just (obj, t)
    Just min_intersect@(_, min_pos) -> if t < 0 then Just min_intersect
                                       else if t < min_pos then Just (obj, t)
                                             else Just min_intersect

shader :: Ray -> [Object] -> [Light] -> Color
shader _ [] _                                   = Vec3 127.5 127.5 127.5
shader ray@(Ray origin direction) objs lights = case checkIntersect ray objs of
     Nothing                 -> Vec3 127.5 127.5 127.5
     Just (min_obj, min_pos) -> phong ray point min_obj objs lights where
       point = plus origin (multScaler direction min_pos)

reflection :: Ray -> [Surface] -> [Object] -> Color
reflection = error "Not Implemented"

refraction :: Ray -> [Surface] -> [Object] -> Color
refraction = error "Not Implemented"

clampVec :: Vec3 -> Vec3 -> Vec3 -> Vec3
clampVec (Vec3 minx miny minz) (Vec3 x y z) (Vec3 maxx maxy maxz)
  = Vec3 (clamp minx x maxx) (clamp miny y maxy) (clamp minz z maxz)

clamp :: Double -> Double -> Double -> Double
clamp min x max
 | x < min = min
 | x > max = max
 | otherwise = x

-- Given ray and depth, compute lighting, namely local + reflection + refraction
trace :: Ray -> [Surface] -> [Object] -> [Light] -> Int -> Color
trace ray@(Ray _ _) _ objs lights depth =
  if depth > 20 then Vec3 127.5 127.5 127.5
  else shader ray objs lights

-- Perform view transformation similar to glm::lookAt
viewTransform :: Camera -> Vector3 Vec3
viewTransform (Camera pos at up _) = Vector3 cx cy cz where
  cz = normalize (minus at pos)
  cx = normalize (cross up cz)
  cy = cross cz cx

{-
-- Given the Image width and height, the View Coordinates, camera fovy angle, internally call trace function and returns a matrix(2D array) of image_data.
sendRay :: Image -> Camera -> M.Matrix Vec3
-- sendRay (Image width height) (Vec3 x y z) (Camera _ _ _ fovy) =
sendRay image@(Image width height) camera@(Camera pos at up fovy)
  = let view@(Vec3 cx cy cz) = viewTransform camera
        aspectratio = width / fromIntegral height
        h = 2 * tan (radians (0.5 * fovy))
        w = h * aspectratio
    in
-}
-- sendRay = error "Not Implemented"
