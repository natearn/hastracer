module Octree where

import Algebra
import Data.Maybe (mapMaybe,isJust,isNothing,catMaybes,fromMaybe,fromJust,listToMaybe)
import Data.List (minimumBy,foldl',mapAccumR)

import Debug.Trace
traceAlong x = traceShow x x

data Box = Box Point Point deriving Show
data Octree a = Octree { bounds :: Box, objects :: [a], subtrees :: [Octree a] }

-- Box operations

-- not efficient box intersection
instance Intersectable Box where
	intersection b@(Box p1 p2) r@(o,d)
		| null ts = Nothing
		| otherwise = Just $ minimumBy d ts
		where
			tx1 = intersection (Plane p1 (-1,0,0)) r
			tx2 = intersection (Plane p2 (1,0,0)) r
			ty1 = intersection (Plane p1 (0,-1,0)) r
			ty2 = intersection (Plane p2 (0,1,0)) r
			tz1 = intersection (Plane p1 (0,0,1)) r
			tz2 = intersection (Plane p2 (0,0,-1)) r
			ts = filter (\(x,_) -> within b x) $ catMaybes [tx1,ty1,tz1,tx2,ty2,tz2]
			d (a,_) (b,_) = compare (dist a o) (dist b o)

within :: Box -> Point -> Bool
within (Box (x1,y1,z1) (x2,y2,z2)) (a,b,c) = (g x1 a x2) && (g y1 b y2) && (g z1 c z2)
	where
	f a b = epsilon (b - a) || a < b
	g a b c = f a b && f b c

contains :: Box -> Triangle -> Bool
contains b@(Box l u) (Triangle (Vertex p1 _) (Vertex p2 _) (Vertex p3 _)) =
	all (within b) [p1,p2,p3]

subdivide :: Box -> [Box]
subdivide (Box l@(xl,yl,zl) u@(xu,yu,zu)) =
	[
		Box l m,
		Box (l `add` x) (m `add` x),
		Box (l `add` y) (m `add` y),
		Box (l `add` x `add` y) (m `add` x `add` y),
		Box (l `add` z) (m `add` z),
		Box (l `add` x `add` z) (m `add` x `add` z),
		Box (l `add` y `add` z) (m `add` y `add` z),
		Box m u
	]
	where
		d@(dx,dy,dz) = (1/2) `mult` (u `sub` l)
		m = l `add` d
		x = (dx,0,0)
		y = (0,dy,0)
		z = (0,0,dz)

minBox :: [Point] -> Box
minBox (p:ps) = f p p ps
	where
		f l u [] = Box l u
		f l u (p:ps) = f (g min l p) (g max u p) ps
		g c (x1,y1,z1) (x2,y2,z2) = (c x1 x2,c y1 y2,c z1 z2)

-- Octree operations

instance (Show a) => Show (Octree a) where
	show (Octree box obs subs) = unwords ["Octree ",show box,show obs,show $ length subs]

size :: Octree a -> Int
size (Octree _ ts subs) = length ts + (sum $ map size subs)

insertOctree :: Octree Triangle -> Triangle -> Maybe (Octree Triangle)
insertOctree (Octree b ts subs) t
	| not $ contains b t = Nothing
	| fits               = Just $ Octree b ts os'
	| otherwise          = Just $ Octree b (t:ts) subs
	where
		os = if null subs then mksubs b else subs
		(fits,os') = mapAccumR f False os
		f True o = (True,o)
		f False o = maybe (False,o) ((,)True) (insertOctree o t)
		mksubs :: Box -> [Octree Triangle]
		mksubs b = map (\s -> Octree s [] []) $ subdivide b

searchOctree :: Octree a -> Ray -> [a]
searchOctree (Octree b ts subs) r
	| isJust $ intersection b r =
		ts ++ (concatMap (\o -> searchOctree o r) subs)
	| otherwise = []

-- scan the octree but don't intersect the triangles
probeOctree :: Octree a -> Ray -> Maybe (Point,Int)
probeOctree (Octree b _ subs) r
	| isNothing x = Nothing
	| isNothing y = fmap (\(p,n) -> (p,0)) x
	| otherwise   = fmap (\(p,d) -> (p,d+1)) y
	where
		x = intersection b r
		y = listToMaybe $ mapMaybe (flip probeOctree r) subs

-- ignores failures
buildOctree :: [Triangle] -> Octree Triangle
buildOctree ts = foldl' (\o t -> fromMaybe o $ insertOctree o t) (Octree mbox [] []) ts
	where
		mbox = minBox $ concatMap points ts

instance (Intersectable a) => Intersectable (Octree a) where
	intersection o r@(p,d)
		| null res  = Nothing
		| otherwise = Just $ minimumBy (\a b -> compare (dist a) (dist b)) res
		where
			res = mapMaybe (flip intersection r) $ searchOctree o r
			dist (x,_) = norm $ x `sub` p
