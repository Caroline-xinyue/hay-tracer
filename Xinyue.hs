module Xinyue where

import DataTypes
import qualified Data.Vector as V
import qualified Data.Matrix as M

-- Given ray, determine if the ray intersects with any object
intersect :: Ray -> Int
intersect = error "Not Implemented"

-- Calculate the color of a point on an object based on the Phong reflection model
phong :: Ray -> Point -> Object -> Color
phong = error "Not Implemented"

-- Given ray and depth, compute lighting, namely local + reflection + refraction
trace :: Ray -> Int -> Color
trace = error "Not Implemented"

-- Perform view transformation similar to glm::lookAt
viewTransform :: Camera -> Vec3
viewTransform = error "Not Implemented"

-- Given the Image width and height, the View Coordinates, camera fovy angle, internally call trace function and returns a matrix(2D array) of image_data.
sendRay :: Image -> Vec3 -> Double -> M.Matrix Vec3
sendRay = error "Not Implemented"
