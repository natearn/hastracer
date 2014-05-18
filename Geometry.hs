module Geometry where

import Data.Vect.Double
import Data.List (permutations,or,minimumBy)
import Data.Maybe (mapMaybe,isJust,catMaybes)
import Control.Monad (guard)

instance Eq Vec3 where
	(Vec3 a b c) == (Vec3 x y z) = a == x && b == y && c == z

class Intersectable a where
	intersection :: a -> Ray -> Maybe (Vec3,Normal3)

data Ray = Ray Vec3 Normal3 deriving Show
data Plane = Plane Vec3 Normal3 deriving Show
data Triangle = Triangle Vec3 Vec3 Vec3 deriving Show
data Box = Box Vec3 Vec3 deriving Show
data Sphere = Sphere Vec3 Double deriving Show

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
		-- | epsilon $ n &. (o &- p) = Just (o,u) -- already on the plane
		| epsilon $ n &. (fromNormal d) = Nothing -- parellel to plane
		| t < eps = Nothing -- can't intersect behind the ray
		| otherwise = Just (o &+ (fromNormalRadius t d), u)
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

instance Intersectable Sphere where
	intersection (Sphere c r) (Ray o l) =
		fmap (\v -> (v,mkNormal $ v &- c)) $ fmap (\d -> o &+ (fromNormalRadius d l)) $ isect =<< quadroot qa qb qc
		where
			v = fromNormal l
			qa = v &. v
			qb = 2 * (v &. (o &- c))
			qc = ((o &- c) &. (o &- c)) - (r^2)
			-- this calculates the closest intersection distance that is greater than epsilon
			isect (x,y)
				| x < eps && y < eps = Nothing
				| x < eps            = Just y
				| y < eps            = Just x
				| otherwise          = Just (minimum [x,y])

--find the roots of a quadratic polynomial
quadroot a b c
	| d < 0 = Nothing
	| otherwise = Just (x,y)
	where
		x = e + sqrt d / (2 * a)
		y = e - sqrt d / (2 * a)
		d = b ^ 2 - (4 * a * c)
		e = - b / (2 * a)
