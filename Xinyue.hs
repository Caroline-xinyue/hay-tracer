module Xinyue where

import DataTypes
import Util
import Rachel
import qualified Data.Vector as V
import qualified Data.Matrix as M

-- Calculate the color of a point on an object based on the Phong reflection model
phong :: Ray -> Point -> Object -> [Object] -> [Light] -> [Surface] -> [Pigment] -> Color
{-
phong ray@(Ray origin direction) intersectPt intersectObj objs []
  = Vec3 127.5 127.5 127.5
phong ray@(Ray origin direction) intersectPt intersectObj@(Solid _ _ pigmentIdx surfaceIdx) objs (Light _ color _ : lights)
  = let (ka, kd, ks, alpha) = getSurfaceParam surfaces surfaceIdx
        normal  = getNormal intersectObj intersectPt
        diffuse = getColor (pigments !! pigmentIdx) intersectPt
        finalColor = 0.1 * color * ka
-}
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

shader :: Ray -> [Object] -> [Light] -> [Surface] -> [Pigment] -> Color
shader _ [] _ _ _                                               = Vec3 127.5 127.5 127.5
shader ray@(Ray origin direction) objs lights surfaces pigments = case checkIntersect ray objs of
     Nothing                 -> Vec3 127.5 127.5 127.5
     Just (min_obj, min_pos) -> phong ray point min_obj objs lights surfaces pigments where
       point = plus origin (multScaler direction min_pos)

reflection :: Ray -> [Surface] -> [Object] -> Color
reflection = error "Not Implemented"

refraction :: Ray -> [Surface] -> [Object] -> Color
refraction = error "Not Implemented"

-- Given ray and depth, compute lighting, namely local + reflection + refraction
trace :: Ray -> [Surface] -> [Object] -> [Light] -> [Pigment] -> Int -> Color
trace ray@(Ray _ _) surfaces objs lights pigments depth =
  if depth > 20 then Vec3 127.5 127.5 127.5
  else clampVec (multScaler (shader ray objs lights surfaces pigments) 255) (Vec3 0 0 0) (Vec3 255 255 255)

-- Perform view transformation similar to glm::lookAt
viewTransform :: Camera -> Vector3 Vec3
viewTransform (Camera pos at up _) = Vector3 cx cy cz where
  cz = normalize (minus at pos)
  cx = normalize (cross up cz)
  cy = cross cz cx

getViewDimension :: Image -> Camera -> Vector2 Double
getViewDimension (Image img_width img_height) (Camera _ _ _ fovy) = Vector2 width height where
  aspectratio = (fromIntegral img_width) / (fromIntegral img_height)
  height      = 2 * tan (radians (0.5 * fovy))
  width       = height * aspectratio

constructRay :: Image -> (Int, Int) -> Camera -> [Surface] -> [Object] -> [Light] -> Ray
constructRay image@(Image img_width img_height) (r, c) camera@(Camera pos _ _ _) _ _ _
  = let (Vector3 cx cy cz)     = viewTransform camera
        (Vector2 width height) = getViewDimension image camera
        pc  = (((fromIntegral c :: Double) / (fromIntegral img_width :: Double)) - 0.5) * width
        pr  = (0.5 - ((fromIntegral r :: Double) / (fromIntegral img_height :: Double))) * height
        dir = normalize (plus (plus (multScaler cx pc) (multScaler cy pr)) (multScaler cz (-1)))
    in Ray pos dir

-- Given the Image width and height, the View Coordinates, camera fovy angle, internally call trace function and returns a matrix(2D array) of image_data.
sendRay :: Image -> Camera -> [Surface] -> [Object] -> [Light] -> [Pigment] -> M.Matrix Color
sendRay image@(Image img_width img_height) camera surfaces objects lights pigments
  = sendRayPixel image_data image (img_height - 1, img_width - 1) camera surfaces objects lights pigments where
  sendRayPixel :: M.Matrix Color -> Image -> (Int, Int) -> Camera -> [Surface] -> [Object] -> [Light] -> [Pigment] -> M.Matrix Color
  sendRayPixel mat image pixel@(0, 0) camera surfaces objects lights pigments
    = let ray   = constructRay image pixel camera surfaces objects lights
          color = trace ray surfaces objects lights pigments 0
      in writePixel mat 0 0 color
  sendRayPixel mat image@(Image img_width _) pixel@(r, 0) camera surfaces objects lights pigments
    = let ray         = constructRay image pixel camera surfaces objects lights
          color       = trace ray surfaces objects lights pigments 0
          mat_written = writePixel mat r 0 color
      in sendRayPixel mat_written image (r - 1, img_width - 1) camera surfaces objects lights pigments
  sendRayPixel mat image pixel@(r, c) camera surfaces objects lights pigments
    = let ray         = constructRay image pixel camera surfaces objects lights
          color       = trace ray surfaces objects lights pigments 0
          mat_written = writePixel mat r c color
      in sendRayPixel mat_written image (r, c - 1) camera surfaces objects lights pigments

image_data = M.fromList size size (replicate (size * size) (Vec3 255 0 0))
