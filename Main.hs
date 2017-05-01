module Main where

import Rachel
import Xinyue
import Prelude
import DataTypes
import System.IO
import System.Environment
import Control.Monad

main = do
  (input:_)    <- getArgs
  handle       <- openFile input ReadMode
  fileName     <- hGetLine handle
  dimensions   <- hGetLine handle
  cameraInputs <- replicateM 4 (hGetLine handle)
  lightNum     <- hGetLine handle
  lightInputs  <- replicateM (read lightNum :: Int) (hGetLine handle)
  pigNum       <- hGetLine handle
  pigInputs    <- replicateM (read pigNum :: Int) (hGetLine handle)
  sfNum        <- hGetLine handle
  sfInputs     <- replicateM (read sfNum :: Int) (hGetLine handle)
  objNum       <- hGetLine handle
  objInputs    <- replicateM (read objNum :: Int) (hGetLine handle)
  let image    = readImage dimensions
      camera   = readCamera cameraInputs
      lights   = readLights lightInputs
      pigments = readPigments pigInputs
      surfaces = readSurfaces sfInputs
      objects  = readObjects objInputs
      img_data = sendRay image camera objects lights surfaces pigments
  writePPM6 fileName image img_data
