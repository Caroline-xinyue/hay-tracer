module Rachel where

import Prelude
import DataTypes
import Data.Word
import System.IO
import qualified Data.Vector as V
import qualified Data.Matrix as M
import qualified Data.ByteString.Lazy as BIN
import qualified Data.ByteString.Char8 as DBC8
-- import qualified Data.ByteString.Conversion as DBC
-- import qualified Data.Double.Conversion.ByteString as DD

-- Read input file into various GADTs
readObjects :: String -> [Object]
readObjects = error "Not Implemented"

readPigments :: String -> [Pigment]
readPigments = error "Not Implemented"

readSurfaces :: String -> [Surface]
readSurfaces = error "Not Implemented"

readLights :: [String] -> [Light]
readLights [] = []
readLights (x : xs) = light : readLights xs where
  vecs = strToVec3s x
  light = Light (vecs !! 0) (vecs !! 1) (vecs !! 2)

readImage :: String -> Image
readImage = error "Not Implemented"

readCamera :: [String] -> Camera
readCamera (camera : at : up : fovy : []) = Camera c a u f where
  c = head $ strToVec3s camera
  a = head $ strToVec3s at
  u = head $ strToVec3s up
  f = read fovy :: Double
readCamera _ = error "camera data corrupted"

strToVec3s :: String -> [Vec3]
strToVec3s = groupDoubles . strToDoubles

-- "1.1 2.2" -> [1.1, 2.2]
strToDoubles :: String -> [Double]
strToDoubles str = map read (words str) :: [Double]

-- [1.1, 2.2, 3.3, 4.4, 5.5, 6.6] -> [(Vec3 1.1 2.2 3.3), (Vec3 4.4, 5.5, 6.6)]
groupDoubles :: [Double] -> [Vec3]
groupDoubles ds
  | length ds >= 3 = doublesToVec3 (take 3 ds) : (groupDoubles (drop 3 ds))
  | otherwise = []

-- [1.1, 2.2, 3.3] -> Vec3 1.1 2.2 3.3
doublesToVec3 :: [Double] -> Vec3
doublesToVec3 ds
  | length ds == 3 = Vec3 (ds !! 0) (ds !! 1) (ds !! 2)
  | otherwise = error "list length is not 3"

-- Given ray, a specific object, calculate the intersection distance from ray origin in view coordinates.
getIntersect :: Ray -> Object -> Double
getIntersect = error "Not Implemented"

-- Given a specific object and the point on object, compute the normal vector
getNormal :: Object -> Point -> Vec3
getNormal = error "Not Implemented"


-- ========================================================================
-- TODO: Change function names to match the actual arguments
-- Given the matrix(2D array) of image_data, produce image in PPM P6 format
write_ppm6 :: String -> Image -> M.Matrix Color -> IO()
write_ppm6 str img mat = writePPM str img (doublesToWords (matrixToList mat))

stringPPM :: Image -> [(BIN.ByteString,BIN.ByteString,BIN.ByteString)] -> BIN.ByteString
stringPPM image ps =
  BIN.pack (map (fromIntegral . fromEnum) $ "P3\n" ++ show (getWidth image) ++ " " ++ show (getHeight image) ++ "\n255\n") `BIN.append`
  BIN.concat (map (\(r,g,b) -> BIN.concat [r, space, g, space, b, space]) ps) where
    space = BIN.pack [0x20]

writePPM :: String -> Image -> [(BIN.ByteString,BIN.ByteString,BIN.ByteString)] -> IO ()
writePPM f sz ps = BIN.writeFile f (stringPPM sz ps)

writePixel :: M.Matrix Color -> Int -> Int -> Color -> M.Matrix Color
writePixel mat x y c = M.setElem c (x, y) mat

getWidth :: Image -> Int
getWidth (Image x _) = x

getHeight :: Image -> Int
getHeight (Image _ y) = y

matrixToList :: M.Matrix Color -> [(Double, Double, Double)]
matrixToList m = concatMap vec3ListToTuple (M.toLists m)

vec3ListToTuple :: [Vec3] -> [(Double, Double, Double)]
vec3ListToTuple [] = []
vec3ListToTuple (x : xs) = vec3ToTuple x : vec3ListToTuple xs

vec3ToTuple :: Vec3 -> (Double, Double, Double)
vec3ToTuple (Vec3 x y z) = (x, y, z)

doublesToWords :: [(Double, Double, Double)] -> [(BIN.ByteString,BIN.ByteString,BIN.ByteString)]
doublesToWords ds = map (\(d1, d2, d3) -> (dToB d1, dToB d2, dToB d3)) ds

dToB :: Double -> BIN.ByteString
dToB d = BIN.fromChunks [DBC8.pack (show d)]

fileName = "haha.ppm"
size = 1000
image = Image size size
mat = M.fromList size size (replicate (size * size) (Vec3 255 0 0))
mat2 = writePixel mat 1 1 (Vec3 0 255 0)
