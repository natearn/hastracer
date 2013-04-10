
import Data.Vect.Double
import Data.List (permutations,or,minimumBy)
import Data.Maybe (mapMaybe,isJust)
import Control.Monad (guard)

class Intersectable a where
	intersection :: a -> Ray -> Maybe (Vec3,Vec3)

data Triangle = Triangle Vec3 Vec3 Vec3 deriving Show
data Plane = Plane Vec3 Normal3 deriving Show
data Ray = Ray Vec3 Vec3 deriving Show

eps :: Double
eps = 0.000001 -- adjust this as needed

epsilon :: Double -> Bool
epsilon x = x < eps && x > (-eps)

vertices :: Triangle -> [Vec3]
vertices (Triangle a b c) = [a,b,c]

normal :: Triangle -> Normal3
normal (Triangle a b c) = mkNormal $ (c &- b) &^ (a &- b)

inside :: [Vec3] -> Vec3 -> Bool
inside (a:b:c:ps) x = f (a:b:c:ps ++ [a]) x
	where
		n = fromNormal $ normal (Triangle a b c)
		check v1 v2 = ((v1 &- v2) &^ (x &- v2)) &. n < eps
		f (v1:v2:vs) x = check v1 v2 && f (v2:vs) x
		f [_] _ = True

instance Intersectable Plane where
	intersection (Plane p u) (Ray o d)
		| epsilon $ n &. (o &- p) = Just (o,n) -- along the plane
		| epsilon $ n &. d = Nothing -- parellel to plane
		| t < eps = Nothing -- can't intersect behind the ray
		| otherwise = Just $ (interpolate t o d, n)
			where
				n = fromNormal u
				t = ((n &. p) - (n &. o)) / (n &. d)

instance Intersectable Triangle where
	intersection t@(Triangle a b c) r = do
		(x,n) <- intersection (Plane a (normal t)) r
		guard $ inside (vertices t) x
		return (x,n)
