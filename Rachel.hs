module Rachel where

import DataTypes

-- Read input file into various GADTs
readObjects :: String -> Vector Object
readObjects = error "Not Implemented"

readPigments :: String -> Vector Pigment
readPigments = error "Not Implemented"

readSurfaces :: String -> Vector Surface
readSurfaces = error "Not Implemented"

readLights :: String -> Vector Light
readLights = error "Not Implemented"

readImage :: String -> Image
readImage = error "Not Implemented"

readCamera :: String -> Camera
readCamera = error "Not Implemented"

-- Given the matrix(2D array) of image_data, produce image in PPM P6 format
write_ppm6 :: String -> Image -> Matrix  -> IO()
write_ppm6 = error "Not Implemented"

-- Given ray, a specific object, calculate the intersection distance from ray origin in view coordinates.
getIntersect :: Ray -> Object -> Double
getIntersect = error "Not Implemented"

-- Given a specific object and the point on object, compute the normal vector
getNormal :: Object -> Point -> Vec3
getNormal = error "Not Implemented"
