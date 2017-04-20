module DataTypes where

import Prelude

type String = [Char]
type Point = Vec
type Color = Vec

data Vec = Vec Double Double Double
-- Sphere Vec (center) Double (radius) | Plane Vec (coefficients)
data Object = Sphere Vec Double | Plane Vec
-- Camera Point (camera pos) Point (at point) Vec (up vector) Double (fovy)
data Camera = Camera Point Point Vec Double
-- Image Int (width) Int (height)
data Image = Image Int Int
-- Light Point (source) Color (intensity) Vecc3 (attenuation)
data Light = Light Point Color Vec
-- Ray Point (origin) Vec (direction)
data Ray = Ray Point Vec
-- Solid Color (RGB components) | CheckerBoard Color (color1) Color (color2) Double (square size)
data Pigment = Solid Color | CheckerBoard Color Color Double
-- PhongCoef Double (ambient coef) Double (diffuse coef) Double (specular coef) Double (shininess)
data PhongCoef = PhongCoef Double Double Double Double
-- Surface PhongCoef Double (reflectivity coef) Double (transmission coef) Double (index of refraction)
data Surface = Surface PhongCoef Double Double Double
