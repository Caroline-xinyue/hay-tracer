module Rachel where

import DataTypes
import qualified Data.Vector as V
import qualified Data.Matrix as M

-- Read input file into various GADTs
readObjects :: String -> V.Vector Object
readObjects = error "Not Implemented"

readPigments :: String -> V.Vector Pigment
readPigments = error "Not Implemented"
readSurfaces :: String -> V.Vector Surface
readSurfaces = error "Not Implemented"

readLights :: String -> V.Vector Light
readLights = error "Not Implemented"

readImage :: String -> Image
readImage = error "Not Implemented"

readCamera :: String -> Camera
readCamera = error "Not Implemented"

-- Given the matrix(2D array) of image_data, produce image in PPM P6 format
write_ppm6 :: String -> Image -> M.Matrix Vec3 -> IO()
write_ppm6 = error "Not Implemented"

-- Given ray, a specific object, calculate the intersection distance from ray origin in view coordinates.
getIntersect :: Ray -> Object -> Double
getIntersect = error "Not Implemented"

-- Given a specific object and the point on object, compute the normal vector
getNormal :: Object -> Point -> Vec3
getNormal = error "Not Implemented"
