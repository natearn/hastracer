module Octree where

import Data.Vect.Double
import Geometry
import Data.Maybe (mapMaybe,isJust,isNothing,fromMaybe,listToMaybe)
import Data.List (minimumBy,foldl',mapAccumR)

data Octree a = Octree { bounds :: Box, objects :: [a], subtrees :: [Octree a] }

-- Box operations

contains :: Box -> Triangle -> Bool
contains x@(Box l u) (Triangle a b c) =
	all (inside x) [a,b,c]

subdivide :: Box -> [Box]
subdivide (Box l u) =
	[
		Box l m,
		Box (l &+ x) (m &+ x),
		Box (l &+ y) (m &+ y),
		Box (l &+ x &+ y) (m &+ x &+ y),
		Box (l &+ z) (m &+ z),
		Box (l &+ x &+ z) (m &+ x &+ z),
		Box (l &+ y &+ z) (m &+ y &+ z),
		Box m u
	]
	where
		d@(Vec3 dx dy dz) = (1/2) *& (u &- l)
		m = l &+ d
		x = (Vec3 dx 0 0)
		y = (Vec3 0 dy 0)
		z = (Vec3 0 0 dz)

minBox :: [Vec3] -> Box
minBox (p:ps) = f p p ps
	where
		f l u [] = Box l u
		f l u (p:ps) = f (g min l p) (g max u p) ps
		g c (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = (Vec3 (c x1 x2) (c y1 y2) (c z1 z2))

-- Octree operations

size :: Octree a -> Int
size (Octree _ ts subs) = length ts + (sum $ map size subs)

instance (Show a) => Show (Octree a) where
	show (Octree box obs subs) =
		unwords ["Octree",show box,show (length obs),show subs']
			where subs' = map (show . size) subs

insertOctree :: Octree Triangle -> Triangle -> Maybe (Octree Triangle)
insertOctree (Octree b ts subs) t
	| not $ contains b t = Nothing
	| fits               = Just $ Octree b ts os'
	| otherwise          = Just $ Octree b (t:ts) subs
	where
		mksubs :: Box -> [Octree Triangle]
		mksubs b = map (\s -> Octree s [] []) $ subdivide b

		f True o = (True,o)
		f False o = maybe (False,o) ((,)True) (insertOctree o t)

		os = if null subs then mksubs b else subs
		(fits,os') = mapAccumR f False os

searchOctree :: Octree a -> Ray -> [a]
searchOctree (Octree b ts subs) r
	| isJust $ intersection b r =
		ts ++ (concatMap (\o -> searchOctree o r) subs)
	| otherwise = []

instance (Intersectable a) => Intersectable (Octree a) where
	intersection o r@(Ray p d)
		| null res  = Nothing
		| otherwise = Just $ minimumBy (\a b -> compare (dist a) (dist b)) res
		where
			res = mapMaybe (flip intersection r) $ searchOctree o r
			dist (x,_) = distance x p

-- remember failures, fill the tree with the triangles that fit
buildOctree :: [Triangle] -> (Octree Triangle,[Triangle])
buildOctree ts = foldl' fun (Octree mbox [] [],[]) ts
	where
		fun (o,fs) t = maybe (o,t:fs) (\x -> (x,fs)) (insertOctree o t)
		mbox = minBox $ concatMap vertices ts
