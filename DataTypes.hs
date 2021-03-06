{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module DataTypes where

import Prelude
import Control.DeepSeq
import GHC.Generics

type Point = Vec3
type Color = Vec3

data Vec3 = Vec3 Double Double Double
  deriving (Show, Generic, NFData)
data Vec4 = Vec4 Double Double Double Double
  deriving Show

data Vector2 a = Vector2 a a
  deriving Show
data Vector3 a = Vector3 a a a
  deriving Show

-- Sphere Vec3 (center) Double (radius) Int (np) Int (nf) | Plane Vec4 (coefficients)
data Object = Sphere { getCenter :: Vec3, getRadius :: Double, getNp :: Int, getNf :: Int }
            | Plane { getCoef :: Vec4, getNp :: Int, getNf :: Int }
  deriving Show
-- Camera Point (camera pos) Point (at point) Vec3 (up Vec3tor) Double (fovy)
data Camera = Camera Point Point Vec3 Double
  deriving Show
-- Image Int (width) Int (height)
data Image = Image Int Int
  deriving Show
-- Light Point (source) Color (intensity) Vec3c (attenuation)
data Light = Light { getPt :: Point, getCol :: Color, getAtt :: Vec3 }
  deriving Show
-- Ray Point (origin) Vec3 (direction)
data Ray = Ray Point Vec3
  deriving Show
-- Solid Color (RGB components) | CheckerBoard Color (color1) Color (color2) Double (square size)
data Pigment = Solid Color | CheckerBoard Color Color Double
  deriving Show
-- PhongCoef Double (ambient coef) Double (diffuse coef) Double (specular coef) Double (shininess)
data PhongCoef = PhongCoef { getKa :: Double
                           , getKd :: Double
                           , getKs :: Double
                           , getAlpha :: Double
                           }
  deriving Show
-- Surface PhongCoef Double (reflectivity coef) Double (transmission coef) Double (index of refraction)
data Surface = Surface { getPhongCoef :: PhongCoef
                       , getKr :: Double
                       , getKt :: Double
                       , getKi :: Double
                       }
  deriving Show
