module Geometry where

import Data.Vect.Double
import Data.List (permutations,or,minimumBy)
import Data.Maybe (mapMaybe,isJust,catMaybes)
import Control.Monad (guard)

class Intersectable a where
	intersection :: a -> Ray -> Maybe (Vec3,Vec3)

data Ray = Ray Vec3 Normal3 deriving Show
data Plane = Plane Vec3 Normal3 deriving Show
data Triangle = Triangle Vec3 Vec3 Vec3 deriving Show
data Box = Box Vec3 Vec3 deriving Show

eps :: Double
eps = 0.000001 -- adjust this as needed

epsilon :: Double -> Bool
epsilon x = x < eps && x > (-eps)

vertices :: Triangle -> [Vec3]
vertices (Triangle a b c) = [a,b,c]

normal :: Triangle -> Normal3
normal (Triangle a b c) = mkNormal $ (c &- b) &^ (a &- b)

onPoly :: [Vec3] -> Vec3 -> Bool
onPoly (a:b:c:ps) x = f (a:b:c:ps ++ [a]) x
	where
		n = fromNormal $ normal (Triangle a b c)
		check v1 v2 = ((v1 &- v2) &^ (x &- v2)) &. n < eps
		f (v1:v2:vs) x = check v1 v2 && f (v2:vs) x
		f [_] _ = True

inside :: Box -> Vec3 -> Bool
inside (Box (Vec3 lx ly lz) (Vec3 ux uy uz)) (Vec3 px py pz) =
	lx <= px + eps && px <= ux + eps &&
	ly <= py + eps && py <= uy + eps &&
	lz <= pz + eps && pz <= uz + eps

instance Intersectable Plane where
	intersection (Plane p u) (Ray o d)
		| epsilon $ n &. (o &- p) = Just (o,n) -- along the plane
		| epsilon $ n &. (fromNormal d) = Nothing -- parellel to plane
		| t < eps = Nothing -- can't intersect behind the ray
		| otherwise = Just (o &+ (fromNormalRadius t d), n)
			where
				n = fromNormal u
				t = ((n &. p) - (n &. o)) / (n &. (fromNormal d))

instance Intersectable Triangle where
	intersection t@(Triangle a b c) r = do
		(x,n) <- intersection (Plane a (normal t)) r
		guard $ onPoly (vertices t) x
		return (x,n)

instance Intersectable Box where
	intersection b@(Box l u) r@(Ray o _)
		| null ts = Nothing
		| otherwise = Just $ minimumBy f ts'
		where
			ts = catMaybes [
				intersection (Plane l (mkNormal (Vec3 (-1) 0 0))) r,
				intersection (Plane l (mkNormal (Vec3 0 (-1) 0))) r,
				intersection (Plane l (mkNormal (Vec3 0 0 1))) r,
				intersection (Plane u (mkNormal (Vec3 1 0 0))) r,
				intersection (Plane u (mkNormal (Vec3 0 1 0))) r,
				intersection (Plane u (mkNormal (Vec3 0 0 (-1)))) r
				]
			ts' = filter (\(x,_) -> inside b x) ts
			f (a,_) (b,_) = compare (norm $ a &- o) (norm $ b &- o)
