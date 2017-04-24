module Main where

import Rachel
import Xinyue
import Prelude
import DataTypes
import System.IO
import Control.Monad
-- import qualified Data.Matrix as M

main = do
  fileName <- getLine
  dimensions <- getLine
  cameraInputs <- replicateM 4 getLine
  lightNum <- getLine
  lightInputs <- replicateM (read lightNum :: Int) getLine
  pigNum <- getLine
  pigInputs <- replicateM (read pigNum :: Int) getLine
  sfNum <- getLine
  sfInputs <- replicateM (read sfNum :: Int) getLine
  objNum <- getLine
  objInputs <- replicateM (read objNum :: Int) getLine
  putStrLn fileName
  print (readImage dimensions)
  print (readCamera cameraInputs)
  print (readLights lightInputs)
  print (readPigments pigInputs)
  print (readSurfaces sfInputs)
  print (readObjects objInputs)
  let image    = readImage dimensions
      camera   = readCamera cameraInputs
      lights   = readLights lightInputs
      pigments = readPigments pigInputs
      surfaces = readSurfaces sfInputs
      objects  = readObjects objInputs
      img_data = sendRay image camera surfaces objects lights pigments
  write_ppm6 fileName image img_data

  -- size     = 100
  -- mat      = M.fromList size size (replicate (size * size) (Vec3 255 0 0)) in
  -- write_ppm3 "hahaha.ppm" (Image size size) mat
