module Algebra where

import Data.List (permutations,or,minimumBy)
import Data.Maybe (mapMaybe,isJust)
import Control.Monad (guard)

class Normal a where
	normal :: a -> Vector

class Intersectable a where
	intersection :: a -> Ray -> Maybe (Point,Vector)
	intersectSegment :: a -> Point -> Point -> Maybe (Point,Vector)
	intersectSegment i a b = do
		let r = (a,normalize (b `sub` a))
		(x,n) <- intersection i r
		guard $ (dist x a) < (dist b a)
		return (x,n)

type Scalar = Double
type Vector = (Double,Double,Double)
type Point = (Double,Double,Double)
type Ray = (Point,Vector)
data Plane = Plane Point Vector deriving Show
data Vertex = Vertex { point :: Point, triangles :: [Triangle] } deriving Show
data Triangle = Triangle Vertex Vertex Vertex

-- Triangle operations

instance Show Triangle where
	show (Triangle (Vertex a _) (Vertex b _) (Vertex c _)) =
		"Triangle " ++ (unwords $ map show [a,b,c])

points :: Triangle -> [Point]
points (Triangle (Vertex a _) (Vertex b _) (Vertex c _)) = [a,b,c]

-- Vector Operations

add :: Vector -> Vector -> Vector
add (a,b,c) (x,y,z) = (a+x,b+y,c+z)

sub :: Vector -> Vector -> Vector
sub (a,b,c) (x,y,z) = (a-x,b-y,c-z)

mult :: Scalar -> Vector -> Vector
mult s (a,b,c) = (s*a,s*b,s*c)

norm :: Vector -> Scalar
norm (x,y,z) = sqrt (x^2 + y^2 + z^2)

normalize :: Vector -> Vector
normalize v = mult (1/norm v) v

-- this fails when vectors facing exact opposite direction
average :: [Vector] -> Vector
average vs = mult (1/fromIntegral(length vs)) (foldr add (0,0,0) vs)

inside :: [Point] -> Point -> Bool
inside (a:b:c:ps) x = f (a:b:c:ps ++ [a]) x
	where
		n = normalize $ (c `sub` b) `cross` (a `sub` b)
		check v1 v2 = ((v1 `sub` v2) `cross` (x `sub` v2)) `dot` n < 0
		f (v1:v2:vs) x = check v1 v2 && f (v2:vs) x
		f [_] _ = True

-- https://en.wikipedia.org/wiki/Cross_product#Conversion_to_matrix_multiplication
cross :: Vector -> Vector -> Vector
cross (a1,a2,a3) (b1,b2,b3) = (x,y,z)
	where
		x = a2 * b3 - a3 * b2
		y = a3 * b1 - a1 * b3
		z = a1 * b2 - a2 * b1

dot :: Vector -> Vector -> Scalar
dot (a,b,c) (x,y,z) = a*x + b*y + c*z

dist :: Point -> Point -> Scalar
dist a b = norm $ a `sub` b

reflect :: Vector -> Vector -> Vector
reflect v n = (2 * (v' `dot` n)) `mult` n `sub` v' -- reflection vector
	where v' = normalize $ (-1) `mult` v

-- Normals

instance Normal Vertex where
	normal (Vertex _ ts) = normalize $ average $ map normal ts

instance Normal Triangle where
	normal (Triangle (Vertex p1 _) (Vertex p2 _) (Vertex p3 _)) =
		normalize $ cross (p3 `sub` p2) (p1 `sub` p2)

instance Normal Plane where
	normal (Plane _ n) = n

-- Intersections

epsilon :: (Fractional a, Ord a) => a -> Bool
epsilon x = x < e && x > (-e)
	where e = 0.00001 -- adjust this as needed

instance Intersectable Plane where
	intersection (Plane p n) (o,d)
		| epsilon $ n `dot` d = Nothing -- parellel to plane
		| epsilon t = Nothing -- can't intersect with things really close
		| t < 0     = Nothing -- can't intersect with things behind the ray
		| otherwise = Just $ (o `add` (t `mult` d),n)
			where
				t = ((n `dot` p) - (n `dot` o)) / (n `dot` d)

{-
-- http://www.scratchapixel.com/lessons/3d-basic-lessons/lesson-9-ray-triangle-intersection/m-ller-trumbore-algorithm/
instance Intersectable Triangle where
	intersection (Triangle (Vertex a _) (Vertex b _) (Vertex c _)) (o,d) =
		if (epsilon $ d `dot` n) -- parallel
		|| u < 0 || v < 0 || u + v > 1 || epsilon t || t <= 0
		then Nothing
		else Just $ o `add` (t `mult` d)
		where
			i = o `sub` a
			j = b `sub` a
			k = c `sub` a
			l = d `cross` k
			m = i `cross` j
			n = j `cross` k -- normal can be used to cull back-facing intersections
			(t,u,v) = (1 / (j `dot` l)) `mult` (m `dot` k , l `dot` i , m `dot` d)
-}

instance Intersectable Triangle where
	intersection t r = do
		(x,n) <- intersection (Plane (head $ points t) (normal t)) r
		guard $ inside (points t) x
		return (x,n)


-- lists of intersectables are intersectable
instance (Intersectable a) => Intersectable [a] where
	intersection xs r@(p,d)
		| null res  = Nothing
		| otherwise = Just $ minimumBy (\a b -> compare (dist a) (dist b)) res
		where
			res = mapMaybe (flip intersection r) xs
			dist (x,_) = norm $ x `sub` p

-- inefficient and incomplete triangle triangle intersection algorithm
-- TODO: coplanarTriangleIntersection
dumbTrisect :: Triangle -> Triangle -> Bool
dumbTrisect
	a@(Triangle (Vertex p1 _) (Vertex p2 _) (Vertex p3 _))
	b@(Triangle (Vertex q1 _) (Vertex q2 _) (Vertex q3 _))
	= test [p1,p2,p3] b || test [q1,q2,q3] a
	where
		test ps t = or $ mapMaybe (rayCheck t) $ map (take 2) $ permutations ps
		rayCheck tri [a,b] = do
			(c,n) <- intersection tri (a,(normalize $ b `sub` a))
			return $ c `sub` a <= b `sub` a

{-
coplanar :: Triangle -> Triangle -> Bool
overlap :: Triangle -> Triangle -> Bool
overlap t1 t2 = if overlap t1 t2 then project onto largest axiskgg
-}


{-
iterate over ALL vertices, extend each one along the normal by some amount

- iterate over the mesh to generate each vertex's initial step size
- iterate over the mesh, attempting to move each vertex out by its step size
	EACH - move the vertex out, then intersection check with mesh

- Need to know which vertices can still move and which ones can't

envelope :: [Vertex] -> [(Vector,Vector)]
iteration [(Vertex,(Vector,Vector))] eps
epsilon :: Vertex -> Vector
envelope :: Triangle -> (Triangle,Triangle)
-}

{-
testbox = ((0,0,0),(1,1,1)) :: Box
testray1 = ((0,-1,0),(0,1,0)) :: Ray
testray2 = ((2,0,0),(0,1,0)) :: Ray
testray3 = ((-1,-0.5,-1),(1,1,1)) :: Ray
-}

{- TEST
v1 = Vertex (-2,0,0) []
v2 = Vertex (2,0,0) []
v3 = Vertex (0,2,0) []
t1 = Triangle v1 v2 v3
r = ((0,2,-1),normalize (0,0,1)) :: Ray

u1 = Vertex (-2,0,1) []
u2 = Vertex (2,0,-1) []
u3 = Vertex (0,2,1) []
t2 = Triangle u1 u2 u3
-}

{-
https://duckduckgo.com/k/?u=http%3A%2F%2Fwww.cis.temple.edu%2F~lakamper%2Fcourses%2Fcis350_2004%2Fetc%2Fmoeller_triangle.pdf
trisect :: Triangle -> Triangle -> Bool
trisect
	a@(Triangle (Vertex p1 _) (Vertex p2 _) (Vertex p3 _))
	b@(Triangle (Vertex q1 _) (Vertex q2 _) (Vertex q3 _))
	= not $ side p || side q || coplanar || not overlap
	where
		np = normal a
		nq = normal b
		dir = np `cross` nq
		dp = -1 `mult` np `dot` p1
		dq = -1 `mult` nq `dot` q1
		p@[dp1,dp2,dp2] = map (\v -> nq `dot` v + dq) [p1,p2,p3]
		q@[dq1,dq2,dq2] = map (\v -> np `dot` v + dp) [q1,q2,q3]
		[pp1,pp2,pp3] = map (dot dir) [p1,p2,p3]
		[pq1,pq2,pq3] = map (dot dir) [q1,q2,q3]
		tp1 = pp1 + ((pp2 - pp1) * (dp1 / (dp1 - dp2)))
		tp2 =
		tq1 = pq1 + ((pq2 - pq1) * (dq1 / (dq1 - dq2)))
		tq2 =
		side xs = all (/= 0) xs && sameSign xs
		sameSign xs = length xs == abs $ sum $ map signum xs
		overlap =
			tp1 < tq1 && tq1 > tp2 || tp1 < tq2 && tq2 > tp2
		coplanar = all (== 0) p && triOverlap a b
		triOverlap a b = TODO
-}
		

