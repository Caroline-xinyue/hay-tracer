module Rachel where

import Util
import Prelude
import DataTypes
import Data.Word
import System.IO
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Matrix as M
import qualified Data.ByteString.Lazy as BIN
import qualified Data.ByteString.Char8 as DBC8
-- import qualified Data.ByteString.Conversion as DBC
-- import qualified Data.Double.Conversion.ByteString as DD

-- ========================================================================
-- get color from pigment
getColor :: Pigment -> Point -> Color
getColor (Solid color) _ = color
getColor (CheckerBoard c1 c2 sideLen) (Vec3 x y z)
  | (pxs + pys + pzs) `mod` 2 == 0 = c1
  | otherwise = c2
  where
    pxs = floor (x / sideLen) :: Int
    pys = floor (y / sideLen) :: Int
    pzs = floor (z / sideLen) :: Int

-- ========================================================================
-- Read input file into various GADTs

readObjects :: [String] -> [Object]
readObjects [] = []
readObjects (x : xs) = obj : readObjects xs where
  nums = strToDoubles (unwords (filter (\str -> str /= "sphere" && str /= "plane") (words x)))
  obj = case nums of
    (a:b:c:d:e:f:_) -> case ((words x) !! 2) of
      "sphere" -> Sphere (Vec3 c d e) f (round a) (round b)
      "plane"  -> Plane (Vec4 c d e f) (round a) (round b)
      _        -> error "unknown object"
    _ -> error "object data corrupted"


readPigments :: [String] -> [Pigment]
readPigments [] = []
readPigments (x : xs) = pig : readPigments xs where
  pig = case ((words x) !! 0) of
    "solid"   -> Solid ((strToVec3s (drop 6 x)) !! 0)
    "checker" -> CheckerBoard ((strToVec3s (drop 8 x)) !! 0) ((strToVec3s (drop 8 x)) !! 1) ((strToDoubles (drop 8 x)) !! 6)
    _         -> error "pigment data corrupted"

readSurfaces :: [String] -> [Surface]
readSurfaces [] = []
readSurfaces (x : xs) = surface : readSurfaces xs where
  doubles = strToDoubles x
  surface = case doubles of
    (a:b:c:d:e:f:g:_) -> Surface (PhongCoef a b c d) e f g
    _                 -> error "surface data corrupted"

readLights :: [String] -> [Light]
readLights [] = []
readLights (x : xs) = light : readLights xs where
  vecs = strToVec3s x
  light = Light (vecs !! 0) (vecs !! 1) (vecs !! 2)

readImage :: String -> Image
readImage str = Image w h where
  dimensions = strToDoubles str
  w = round $ dimensions !! 0
  h = round $ dimensions !! 1

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
  | otherwise      = []

-- [1.1, 2.2, 3.3] -> Vec3 1.1 2.2 3.3
doublesToVec3 :: [Double] -> Vec3
doublesToVec3 ds
  | length ds == 3 = Vec3 (ds !! 0) (ds !! 1) (ds !! 2)
  | otherwise      = error "list length is not 3"

-- =======================================================================

-- Given ray, a specific object, calculate the intersection distance from ray origin in view coordinates.
getIntersect :: Ray -> Object -> Double
getIntersect (Ray origin dir) (Sphere center radius _ _)
  | delta < 0  = -1
  | delta == 0 = let t = -y / (2 * x) in
    if t < 0 then -1 else t
  | otherwise  =
    let t1 = (-y + sqrt(delta)) / (2 * x)
        t2 = (-y - sqrt(delta)) / (2 * x) in
        if t1 < 0 && t2 < 0
          then -1
          else if t1 < 0 && t2 >= 0
            then t2
          else if t1 >= 0 && t2 < 0
            then t1
          else (min t1 t2)
    where
      center_origin = minus origin center
      x = dot dir dir
      y = 2 * (dot center_origin dir)
      z = (dot center_origin center_origin) - radius ** 2
      delta = y ** 2 - 4 * x * z
getIntersect (Ray origin dir) (Plane (Vec4 a b c d) _ _)
  | dot normal dir /= 0 = -((d + (dot origin normal)) / (dot dir normal))
  | otherwise           = -1
  where
    normal = normalize $ Vec3 a b c


-- Given a specific object and the point on object, compute the normal vector
getNormal :: Object -> Point -> Vec3
getNormal (Sphere center _ _ _) point  = normalize $ minus point center
getNormal (Plane (Vec4 a b c _) _ _) _ = normalize $ Vec3 a b c

-- ========================================================================
-- TODO: Change function names to match the actual arguments
-- Given the matrix(2D array) of image_data, produce image in PPM P6 format
write_ppm3 :: String -> Image -> M.Matrix Color -> IO()
write_ppm3 str img mat = writePPM str img (doublesToBStrings (matrixToList mat))

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
vec3ListToTuple []       = []
vec3ListToTuple (x : xs) = vec3ToTuple x : vec3ListToTuple xs

vec3ToTuple :: Vec3 -> (Double, Double, Double)
vec3ToTuple (Vec3 x y z) = (x, y, z)

doublesToBStrings :: [(Double, Double, Double)] -> [(BIN.ByteString,BIN.ByteString,BIN.ByteString)]
doublesToBStrings ds = map (\(d1, d2, d3) -> (dToB d1, dToB d2, dToB d3)) ds

dToB :: Double -> BIN.ByteString
dToB d = BIN.fromChunks [DBC8.pack (show d)]

fileName = "haha.ppm"
size = 1000
image = Image size size
mat = M.fromList size size (replicate (size * size) (Vec3 255 0 0))
mat2 = writePixel mat 1 1 (Vec3 0 255 0)
