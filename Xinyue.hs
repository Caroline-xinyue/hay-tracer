module Xinyue where

import DataTypes
import Util
import Rachel
import Data.List
import Data.Array
import qualified Debug.Trace as TR

calcDiffuse :: Double -> Ray -> Light -> Vec3 -> Vec3 -> Vec3 -> Color
calcDiffuse kd (Ray _ dir) (Light _ light_col (Vec3 a1 a2 a3)) light_dir diffuse normal
  | kd <= 0 = Vec3 0 0 0
  | otherwise =
    let new_normal = if (dot normal dir) > 0 then (multScaler normal (-1)) else normal
        dist_to_light = vlength light_dir
        n_dir = normalize light_dir
        diffuseIntensity = dot n_dir new_normal
        att = a1 + a2 * dist_to_light + a3 * (dist_to_light ** 2)
        attenuation = if att == 0 then 0.0001 else att
    in if diffuseIntensity <= 0 then Vec3 0 0 0
       else multScaler (mult diffuse light_col) (diffuseIntensity * kd * (1.0 / attenuation))

calcSpecular :: Double -> Double -> Ray -> Vec3 -> Vec3 -> Color
calcSpecular ks alpha (Ray _ dir) light_dir normal
  | ks <= 0 = Vec3 0 0 0
  | otherwise =
  let n_dir = TR.trace ("l_dir: " ++ show light_dir) (normalize light_dir)
      specularIntensity = TR.trace ("dir: " ++ show dir ++ "normal: " ++ show normal) ((dot normal (normalize (minus n_dir dir))) ** alpha)
  in if specularIntensity <= 0 then Vec3 0 0 0
     else multScaler (Vec3 1.0 1.0 1.0) ((TR.trace ("specularIntensity: " ++ show specularIntensity) specularIntensity) * ks)

getSurfaceParam :: [Surface] -> Int -> PhongCoef
getSurfaceParam surfaces surfaceIdx = case surfaces !! surfaceIdx of (Surface phongCoef _ _ _) -> phongCoef

checkVisible :: Ray -> Point -> Light -> [Object] -> Bool
checkVisible _ _ _ [] = True
checkVisible shadow_ray intersectPt light@(Light light_pos _ _) (obj : objs)
  = let point = getIntersect shadow_ray obj
    in if point < 0 || point > vdistance intersectPt light_pos then checkVisible shadow_ray intersectPt light objs
       else False

lit :: Ray -> Point -> Object -> [Object] -> [Surface] -> [Pigment] -> Light -> Color
lit ray intersectPt intersectObj objs surfaces pigments light@(Light pos _ _)
  = let (PhongCoef _ kd ks alpha) = getSurfaceParam surfaces (getNf intersectObj)
        normal  = getNormal intersectObj intersectPt
        diffuse = getColor (pigments !! (getNp intersectObj)) intersectPt
        light_dir = minus pos intersectPt
        diffuseCol = calcDiffuse kd ray light light_dir diffuse normal
        specularCol = calcSpecular ks alpha ray light_dir normal
        n_dir = normalize light_dir
        shadow_ray = Ray (plus intersectPt (multScaler n_dir 0.01)) n_dir
        visible = checkVisible shadow_ray intersectPt light objs
    in if visible then TR.trace ("diffuse: " ++ show diffuseCol ++ "specular: " ++ show specularCol) (plus diffuseCol specularCol)
       else Vec3 0 0 0

-- Calculate the color of a point on an object based on the Phong reflection model
phong :: Ray -> Point -> Object -> [Object] -> [Light] -> [Surface] -> [Pigment] -> Color
phong _ _ _ _ [] _ _
  = Vec3 0.5 0.5 0.5
phong ray intersectPt intersectObj objs lights surfaces pigments
  = foldr plus initColor (map litColor (tail lights)) where
      initColor  = multScaler (getCol (head lights)) (0.1 * getKa (getPhongCoef (surfaces !! (getNf intersectObj))))
      litColor = lit ray intersectPt intersectObj objs surfaces pigments

checkIntersect :: Ray -> [Object] -> Maybe (Object, Double)
checkIntersect _ []             = Nothing
checkIntersect ray (obj : objs) = let t = getIntersect ray obj in
  case TR.trace (show t) (checkIntersect ray objs) of
    Nothing                         -> if t < 0 then Nothing
                                       else Just (obj, t)
    Just min_intersect@(_, min_pos) -> if t < 0 then Just min_intersect
                                       else if t < min_pos then Just (obj, t)
                                             else Just min_intersect

shader :: Ray -> [Object] -> [Light] -> [Surface] -> [Pigment] -> Color
shader _ [] _ _ _                                               = Vec3 0.5 0.5 0.5
shader ray@(Ray origin direction) objs lights surfaces pigments = case checkIntersect ray objs of
     Nothing                 -> Vec3 0.5 0.5 0.5
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
  -- else clampVec (multScaler (shader ray objs lights surfaces pigments) 255) (Vec3 0 0 0) (Vec3 255 255 255)
  else clampVec (multScaler (shader ray objs lights surfaces pigments) 255) (Vec3 0 0 0) (Vec3 255 255 255)

-- Perform view transformation similar to glm::lookAt
viewTransform :: Camera -> Vector3 Vec3
viewTransform (Camera pos at up _) = Vector3 cx cy cz where
  cz = multScaler (normalize (minus at pos)) (-1)
  cx = normalize (cross up cz)
  cy = cross cz cx

getViewDimension :: Image -> Camera -> Vector2 Double
getViewDimension (Image img_width img_height) (Camera _ _ _ fovy) = Vector2 width height where
  aspectratio = (fromIntegral img_width) / (fromIntegral img_height)
  height      = 2 * tan (radians (0.5 * fovy))
  width       = height * aspectratio

constructRay :: Image -> (Int, Int) -> Camera -> [Surface] -> [Object] -> [Light] -> Ray
constructRay image@(Image img_width img_height) (r, c) camera@(Camera pos _ _ _) _ _ _
  = let trans@(Vector3 cx cy cz)     = viewTransform camera
        (Vector2 width height) = getViewDimension image camera
        pc  = (((fromIntegral c :: Double) / (fromIntegral img_width :: Double)) - 0.5) * width
        pr  = (0.5 - ((fromIntegral r :: Double) / (fromIntegral img_height :: Double))) * height
        dir = normalize (plus (plus (multScaler cx pc) (multScaler cy pr)) (multScaler cz (-1)))
    in Ray pos dir

-- Given the Image width and height, the View Coordinates, camera fovy angle, internally call trace function and returns a matrix(2D array) of image_data.
sendRay :: Image -> Camera -> [Surface] -> [Object] -> [Light] -> [Pigment] -> Array (Int, Int) Color
sendRay image@(Image img_width img_height) camera surfaces objects lights pigments
  = array ((0, 0), (img_height - 1, img_width - 1)) [((x, y), c) |
                                                       x <- [0..img_height - 1]
                                                     , y <- [0..img_width - 1]
                                                     , let ray = constructRay image (x, y) camera surfaces objects lights
                                                     , let c = trace ray surfaces objects lights pigments 0]
