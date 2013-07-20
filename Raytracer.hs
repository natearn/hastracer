module Raytracer where

import Data.Vect.Double
import Geometry
import Octree
import Data.List (permutations,or,minimumBy,foldl')
import Data.Maybe (mapMaybe,isJust,isNothing,fromJust)
import Codec.Picture -- PNG encoding
import Codec.Picture.Types -- PNG encoding

import Debug.Trace
traceAlong x = traceShow x x

type Colour = Vec3
data Light = Point Vec3 Colour deriving Show
data Material = Phong Colour Colour Double
              | Reflection
              | Bump
              | Texture (Vec3 -> Material)

instance Show Material where
	show (Texture _)   = "Texture"
	show Reflection    = "Reflection"
	show Bump          = "Bump"
	show (Phong a b c) = "Phong " ++ unwords [show a,show b,show c]

data Object a = Object a Material deriving Show

-- no support for rotation or scaling right now
-- ideally Scene has: transformations, object, child scenes
data Scene a = Scene Vec3 (Object a) [Scene a] deriving Show

-- Colour operations

toPixel :: Colour -> PixelRGBF
toPixel (Vec3 r g b) = PixelRGBF (f r) (f g) (f b)
	where f = realToFrac

-- https://en.wikipedia.org/wiki/Phong_lighting
phongLighting :: Vec3
	-> Vec3
	-> Material
	-> Normal3
	-> [Light]
	-> Colour
	-> Colour
phongLighting p e (Phong kd ks shin) n ls amb =
	foldl' combine (kd &! amb) ls
	where
	combine :: Colour -> Light -> Colour
	combine col (Point lp lc)
		| l &. n' < 0 = col
		| r &. v < 0 = col &+ df
		| otherwise  = col &+ df &+ ds
		where
		df = ((l &. n') *& (kd &! lc))
		ds = (((r &. v) ** shin) *& (ks &! lc))
		v = normalize $ e &- p -- direction from point to eye
		l = normalize $ lp &- p -- direction from point to light
		r = reflect n l
		n' = fromNormal n

-- Ray tracing functions

calcRay :: Vec3
	-> Normal3
	-> Normal3
	-> Double
	-> Int
	-> Int
	-> Int
	-> Int
	-> Ray
calcRay eye view up fov width height x y = Ray eye dir
	where
		x' = fromIntegral x
		y' = fromIntegral y
		w' = fromIntegral width
		h' = fromIntegral height
		dist = 100 :: Double
		pl = dist * tan (fov/2) / (h'/2)
		nv = fromNormal view
		nu = fromNormal up
		nr = nv &^ nu
		-- top-left point of screen
		tl = eye &+
			(dist *& nv) &+
			((w'/2 * pl * (-1)) *& nr) &+
			((h'/2 * pl) *& nu)
		dir = mkNormal $
			tl &+ ((x' * pl) *& nr) &+ ((y' * pl * (-1)) *& nu) &- eye

searchScene :: (Intersectable a)
	=> Scene a
	-> Ray
	-> Maybe (Vec3,Normal3,Material)
searchScene (Scene pos (Object v mat) subs) (Ray eye dir) =
	maybe trysubs (\(x,d) -> Just (x,d,mat)) (intersection v ray)
	where
		ray = Ray (eye &- pos) dir
		trysubs = if null res then Nothing else Just $ minimumBy comp res
		res = mapMaybe (flip searchScene ray) subs
		comp (a,_,_) (b,_,_) = compare (distance a eye) (distance b eye)

-- top-level ray cast, handles intersection and colour calculation
castRay :: (Intersectable a)
	=> Ray     -- ray
	-> Scene a -- scene
	-> [Light] -- point lights
	-> Colour  -- ambient lighting
	-> Int     -- reflection limit
	-> Colour  -- pixel value
castRay r@(Ray e d) s ls amb lim = maybe background lighting (searchScene s r)
	where
	-- will want to paramterize background for things like skyboxes
	background = Vec3 0.1 0.1 0.2
	f x = filter (\(Point l _) -> isNothing $ searchScene s (Ray x (mkNormal $ l &- x)))
	refl n d = mkNormal $ reflect n (neg $ fromNormal d)
	lighting (p,n,m@(Phong _ _ _)) = phongLighting p e m n (f p ls) amb
	lighting (p,n,(Texture tmap)) = lighting $ (p,n,tmap p)
	lighting (_,n,Bump) = fromNormal n
	lighting (p,n,Reflection)
		| lim == 0 = background
		| otherwise = castRay (Ray p (refl n d)) s ls amb (lim-1)

render :: (Intersectable a)
	=> Vec3            -- eye
	-> Normal3         -- viewing direction
	-> Normal3         -- the up direction
	-> Double          -- field of view (radians)
	-> Int             -- horizontal resolution (width)
	-> Int             -- vertical resolution (height)
	-> Scene a         -- scene
	-> [Light]         -- point lights
	-> Colour          -- ambient light
	-> Int             -- reflection limit
	-> String          -- output file name
	-> IO ()
render eye view up fov width height scene lights ambient limit name =
	savePngImage name (ImageRGBF image)
	where
		image = generateImage cast width height
		cast x y = toPixel $ castRay (ray x y) scene lights ambient limit
		ray x y = calcRay eye view up fov width height x y
