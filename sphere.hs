module Main where

import Data.Vect.Double
import Geometry
import Octree
import Sphere
import Raytracer

-- parameters
(xres,yres) = (256,256)
view = mkNormal $ Vec3 0 0 (-1)
up =  mkNormal $ Vec3 0 1 0
fov = pi * (2/3) -- 120 degrees
amb = Vec3 0.3 0.3 0.3
limit = 2 :: Int
divisions = 2 :: Int

(tree,fails) = buildOctree $ sphere divisions
eye = (\(Box l u) -> Vec3 0 0 ((distance u l)/2)) $ bounds tree
lights = [ Point (eye &+ (Vec3 2 2 1)) (Vec3 1 1 1) ]
obj = Object tree (Phong (Vec3 0.9 0.5 0.5) (Vec3 0.7 0.3 0.3) 25)
scn = Scene (Vec3 0.1 0 0) obj []

main = render eye view up fov xres yres scn lights amb limit "sphere.png"
