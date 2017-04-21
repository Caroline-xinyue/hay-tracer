module Rachel where

import DataTypes
import qualified Data.Vector as V
import qualified Data.Matrix as M
import Data.Word
import qualified Data.ByteString.Lazy as BIN
import qualified Data.ByteString.Conversion as DBC
import qualified Data.ByteString.Char8 as DBC8
import qualified Data.Double.Conversion.ByteString as DD
import System.IO
import Prelude

-- Read input file into various GADTs
readObjects :: String -> [Object]
readObjects = error "Not Implemented"

readPigments :: String -> [Pigment]
readPigments = error "Not Implemented"

readSurfaces :: String -> [Surface]
readSurfaces = error "Not Implemented"

readLights :: String -> [Light]
readLights = error "Not Implemented"

readImage :: String -> Image
readImage = error "Not Implemented"

readCamera :: String -> Camera
readCamera = error "Not Implemented"

-- Given ray, a specific object, calculate the intersection distance from ray origin in view coordinates.
getIntersect :: Ray -> Object -> Double
getIntersect = error "Not Implemented"

-- Given a specific object and the point on object, compute the normal vector
getNormal :: Object -> Point -> Vec3
getNormal = error "Not Implemented"

-- Given the matrix(2D array) of image_data, produce image in PPM P6 format
write_ppm6 :: String -> Image -> M.Matrix Vec3 -> IO()
write_ppm6 str img mat = writePPM str img (doublesToWords (matrixToList mat))

stringPPM :: Image -> [(Word8,Word8,Word8)] -> BIN.ByteString
stringPPM image ps =
  BIN.pack (map (fromIntegral . fromEnum) $ "P6\n" ++ show (getWidth image) ++ " " ++ show (getHeight image) ++ "\n255\n") `BIN.append`
  BIN.concat (map (\(r,g,b) -> BIN.pack [r,g,b]) ps)

writePPM :: String -> Image -> [(Word8,Word8,Word8)] -> IO ()
writePPM f sz ps = BIN.writeFile f (stringPPM sz ps)

getWidth :: Image -> Int
getWidth (Image x y) = x

getHeight :: Image -> Int
getHeight (Image x y) = y

matrixToList :: M.Matrix Vec3 -> [(Double, Double, Double)]
matrixToList m = concatMap vec3ListToTuple (M.toLists m)

vec3ListToTuple :: [Vec3] -> [(Double, Double, Double)]
vec3ListToTuple [] = []
vec3ListToTuple (x : xs) = vec3ToTuple x : vec3ListToTuple xs

vec3ToTuple :: Vec3 -> (Double, Double, Double)
vec3ToTuple (Vec3 x y z) = (x, y, z)

doublesToWords :: [(Double, Double, Double)] -> [(Word8,Word8,Word8)]
doublesToWords ds = map (\(d1, d2, d3) -> (dToW d1, dToW d2, dToW d3)) ds

dToW :: Double -> Word8
dToW d = fromIntegral (round d) :: Word8

fileName = "haha.ppm"
image = Image 10 10
mat = M.fromList 10 10 (replicate 100 (Vec3 10 10 10))
