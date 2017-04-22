module Main where

import Rachel
import Xinyue
import Prelude
import DataTypes
import System.IO
import Control.Monad

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
  print (readImage dimensions)
  print (readCamera cameraInputs)
  print (readLights lightInputs)
  print (readPigments pigInputs)
  print (readSurfaces sfInputs)
  print (readObjects objInputs)
