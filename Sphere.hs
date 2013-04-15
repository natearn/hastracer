module Sphere (sphere,golf) where

import Data.Vect.Double
import Geometry
import Data.List (foldl')

import Debug.Trace
traceAlong x = traceShow x x

tetrahedron =
	[
		Triangle u d r,
		Triangle d u l,
		Triangle r l u,
		Triangle l r d
	]
	where
		x = 1 / (sqrt 2)
		r = normalize $ Vec3 1 0 (-x)
		l = normalize $ Vec3 (-1) 0 (-x)
		u = normalize $ Vec3 0 1 x
		d = normalize $ Vec3 0 (-1) x

check (Triangle a b c) = all (== (distance a b)) [distance b c,distance c a]

subdiv :: Triangle -> [Triangle]
subdiv (Triangle a b c) =
	[
		Triangle a x z, 
		Triangle b y x, 
		Triangle c z y, 
		Triangle x y z 
	]
	where
		[x,y,z] = map normalize [mid a b, mid b c, mid a c]
		mid p q = p &+ ((1/2) *& (q &- p))

sphere :: Int -> [Triangle]
sphere d = f tetrahedron d
	where
		f :: [Triangle] -> Int -> [Triangle]
		f m 0 = m
		f m d = f (concatMap subdiv m) (d-1)

align :: Triangle -> Vec3 -> Double -> Triangle
align (Triangle a b c) o r = Triangle a' b' c'
	where
	[a',b',c'] = map movePoint [a,b,c]
	movePoint p
		| p == o           = error "p == o"
		| distance p o < r = o &+ (r *& (normalize $ p &- o))
		| otherwise        = p

golf :: Int -> [Triangle]
golf res = foldl' adjust ball controls
	where
		divit = 0.1
		controls = map ((1 + (divit/2)) *&) $ concatMap vertices (sphere 4)
		ball = sphere res
		adjust ts c = map (\t -> align t c divit) ts
		

