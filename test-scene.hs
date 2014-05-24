module Main where

import Data.Vect.Double
import Geometry
import Raytracer

-- parameters
(xres,yres) = (256,256)
view = mkNormal $ Vec3 0 0 (-1)
up =  mkNormal $ Vec3 0 1 0
fov = pi * (2/3) -- 120 degrees
amb = Vec3 0.3 0.3 0.3
limit = 2 :: Int
eye = Vec3 0 0 5
lights = [Point (Vec3 5 5 5) (Vec3 0.9 0.9 0.9)]

m1 = Phong (Vec3 0.5 0.9 0.5) (Vec3 0.3 0.7 0.3) 25
m2 = Phong (Vec3 0.9 0.5 0.5) (Vec3 0.7 0.3 0.3) 25
s1 = Sphere (Vec3 0 0 0) 1
s2 = Sphere (Vec3 (-10) (-10) (-30)) 20
scn = [(Object s1 m1),(Object s2 m2)]

--main = render eye view up fov xres yres scn lights amb limit "test.png"
main = renderScreen eye view up fov xres yres scn lights amb limit
