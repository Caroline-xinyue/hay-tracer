module IO where

import Util
import Prelude
import DataTypes
import Data.Word
import System.IO
import Data.Array
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Matrix as M
import qualified Debug.Trace as TR
import qualified Data.ByteString.Lazy as BIN

-- ========================================================================
-- Read input file into various GADTs

readObjects :: [String] -> [Object]
readObjects []       = []
readObjects (x : xs) = obj : readObjects xs where
  nums = strToDoubles (unwords (filter (\str -> str /= "sphere" && str /= "plane") (words x)))
  obj  = case nums of
    (a:b:c:d:e:f:_) -> case ((words x) !! 2) of
      "sphere" -> Sphere (Vec3 c d e) f (round a) (round b)
      "plane"  -> Plane (Vec4 c d e f) (round a) (round b)
      _        -> error "unknown object"
    _               -> error "object data corrupted"


readPigments :: [String] -> [Pigment]
readPigments []       = []
readPigments (x : xs) = pig : readPigments xs where
  pig = case ((words x) !! 0) of
    "solid"   -> Solid ((strToVec3s (drop 6 x)) !! 0)
    "checker" -> CheckerBoard ((strToVec3s (drop 8 x)) !! 0) ((strToVec3s (drop 8 x)) !! 1) ((strToDoubles (drop 8 x)) !! 6)
    _         -> error "pigment data corrupted"

readSurfaces :: [String] -> [Surface]
readSurfaces []       = []
readSurfaces (x : xs) = surface : readSurfaces xs where
  doubles = strToDoubles x
  surface = case doubles of
    (a:b:c:d:e:f:g:_) -> Surface (PhongCoef a b c d) e f g
    _                 -> error "surface data corrupted"

readLights :: [String] -> [Light]
readLights []       = []
readLights (x : xs) = light : readLights xs where
  vecs  = strToVec3s x
  light = Light (vecs !! 0) (vecs !! 1) (vecs !! 2)

readImage :: String -> Image
readImage str = Image w h where
  dimensions  = strToDoubles str
  w           = round $ dimensions !! 0
  h           = round $ dimensions !! 1

readCamera :: [String] -> Camera
readCamera (camera : at : up : fovy : []) = Camera c a u f where
  c = head $ strToVec3s camera
  a = head $ strToVec3s at
  u = head $ strToVec3s up
  f = read fovy :: Double
readCamera _                              = error "camera data corrupted"

strToVec3s :: String -> [Vec3]
strToVec3s = groupDoubles . strToDoubles

-- "1.1 2.2" -> [1.1, 2.2]
strToDoubles :: String -> [Double]
strToDoubles str = map read (words str) :: [Double]

-- [1.1, 2.2, 3.3, 4.4, 5.5, 6.6] -> [(Vec3 1.1 2.2 3.3), (Vec3 4.4, 5.5, 6.6)]
groupDoubles :: [Double] -> [Vec3]
groupDoubles ds
  | length ds >= 3 = case doublesToVec3 (take 3 ds) of
                       Nothing  -> []
                       Just vec -> vec : (groupDoubles (drop 3 ds))
  | otherwise      = []

-- [1.1, 2.2, 3.3] -> Vec3 1.1 2.2 3.3
doublesToVec3 :: [Double] -> Maybe Vec3
doublesToVec3 ds
  | length ds == 3 = Just (Vec3 (ds !! 0) (ds !! 1) (ds !! 2))
  | otherwise      = Nothing

-- ========================================================================
-- Given the matrix(2D array) of image_data, produce image in PPM P6 format
writePPM6 :: String -> Image -> Array (Int, Int) Color -> IO()
writePPM6 str img mat = writePPM str img (doublesToWords (matrixToList mat))

stringPPM :: Image -> [(Word8,Word8,Word8)] -> BIN.ByteString
stringPPM image ps =
  BIN.pack (map (fromIntegral . fromEnum) $ "P6\n" ++ show (getWidth image) ++ " " ++ show (getHeight image) ++ "\n255\n") `BIN.append`
  BIN.concat (map (\(r,g,b) -> BIN.pack [r, g, b]) ps)

writePPM :: String -> Image -> [(Word8,Word8,Word8)] -> IO ()
writePPM f sz ps = BIN.writeFile f (stringPPM sz ps)

getWidth :: Image -> Int
getWidth (Image x _) = x

getHeight :: Image -> Int
getHeight (Image _ y) = y

matrixToList :: Array (Int, Int) Color -> [(Double, Double, Double)]
matrixToList m = vec3ListToTuple (elems m)

vec3ListToTuple :: [Vec3] -> [(Double, Double, Double)]
vec3ListToTuple []       = []
vec3ListToTuple (x : xs) = vec3ToTuple x : vec3ListToTuple xs

vec3ToTuple :: Vec3 -> (Double, Double, Double)
vec3ToTuple (Vec3 x y z) = (x, y, z)

doublesToWords :: [(Double, Double, Double)] -> [(Word8,Word8,Word8)]
doublesToWords ds = map (\(d1, d2, d3) -> (dToW d1, dToW d2, dToW d3)) ds

dToW :: Double -> Word8
dToW d = fromIntegral (round d)
