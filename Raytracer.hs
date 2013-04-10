module Raytracer where

import Codec.Picture -- PNG encoding
import Codec.Picture.Types -- PNG encoding
import Algebra
import Octree
import Data.List (permutations,or,minimumBy,foldl')
import Data.Maybe (mapMaybe,isJust,isNothing,fromJust)

type Colour = (Double,Double,Double) -- treat like vector
type Light = (Point,Colour) -- don't care about drop-off
data Material = Phong { diffuse :: Colour, specular :: Colour, shininess :: Double }
              | Reflection
              | Texture (Point -> Material)

instance Show Material where
	show (Texture _) = "Texture"
	show Reflection  = "Reflection"
	show (Phong a b c) = "Phong " ++ unwords [show a,show b,show c]

data Object = Object (Octree Triangle) Material
data Scene = Scene Point Object [Scene] -- no support for rotation or scaling right now

instance Intersectable Object where
	intersection (Object t m) r = intersection t r

-- Colour operations

prod :: Colour -> Colour -> Colour
prod (a,b,c) (x,y,z) = (a*x,b*y,c*z)

toPixel :: Colour -> PixelRGBF
toPixel (r,g,b) = PixelRGBF (f r) (f g) (f b)
	where f = realToFrac

phongLighting :: Point -> Point -> Material -> Vector -> [Light] -> Colour -> Colour
phongLighting p e (Phong kd ks shin) n ls amb = foldl' combine (kd `prod` amb) ls
	where
	combine :: Colour -> Light -> Colour -- https://en.wikipedia.org/wiki/Phong_lighting
	combine col (lp, lc) = col `add` ((l `dot` n) `mult` (kd `prod` lc)) `add` (((r `dot` v) ** shin) `mult` (ks `prod` lc))
		where
		l = normalize $ lp `sub` p -- unit vector from point to light
		r = ((2 * (l `dot` n)) `mult` n) `sub` l -- light reflection vector
		v = normalize $ e `sub` p -- unit vector from point to eye

-- Ray tracing functions

calcRay :: Point -> Vector -> Vector -> Double -> Int -> Int -> Int -> Int -> Ray
calcRay eye view up fov width height x y = (eye,dir)
	where
		d = fromIntegral
		dist = 100 :: Double
		pl = dist * tan (fov/2) / (d height/2)
		nr = normalize $ view `cross` up
		nv = normalize view
		nu = normalize up
		tl = eye `add` (dist `mult` nv) `add` ((d width / 2 * pl * (-1)) `mult` nr) `add` ((d height / 2 * pl) `mult` nu) -- top-left point of screen
		dir = normalize $ tl `add` ((d x * pl) `mult` nr) `add` ((d y * pl * (-1)) `mult` nu) `sub` eye

searchScene :: Scene -> Ray -> Maybe ((Point,Vector),Object)
searchScene (Scene pos obj subs) (eye,dir)
	| isJust x = let (p,q) = fromJust x in Just ((p `add` pos,q),obj)
	| null ys  = Nothing
	| otherwise = Just $ minimumBy d ys
	where
		r = (eye `sub` pos,dir)
		x = intersection obj r
		ys = mapMaybe (flip searchScene r) subs
		d ((a,_),_) ((b,_),_) = compare (dist a eye) (dist b eye)

-- top-level ray cast, handles intersection and colour calculation
castRay :: Ray         -- ray
	-> Scene           -- scene
	-> [Light]         -- point lights
	-> Colour          -- ambient lighting
	-> Int             -- reflection limit
	-> Colour          -- pixel value
castRay r@(eye,dir) s ls amb lim = maybe background lighting (searchScene s r)
	where
		background = (0.1,0.1,0.2) -- will want to paramterize this for things like skyboxes
		f x = filter (\(l,_) -> isNothing $ searchScene s (x,normalize $ l `sub` x))
		lighting ((p,n),(Object _ m@(Phong _ _ _))) = phongLighting p eye m n (f p ls) amb
		lighting ((p,n),(Object _ Reflection))
			| lim == 0 = background
			-- | otherwise = castRay (p,reflect dir n) s ls amb (lim-1)
			| otherwise = (0.1 `mult` amb) `add` (0.9 `mult` castRay (p,reflect dir n) s ls amb (lim-1))
		lighting ((p,n),(Object o (Texture tmap))) = lighting $ ((p,n),(Object o (tmap p)))

render :: Point        -- eye
	-> Vector          -- viewing direction
	-> Vector          -- the up direction
	-> Double          -- field of view (radians)
	-> Int             -- horizontal resolution (width)
	-> Int             -- vertical resolution (height)
	-> Scene           -- scene
	-> [Light]         -- point lights
	-> Colour          -- ambient light
	-> Int             -- reflection limit
	-> String          -- output file name
	-> IO ()
render eye view up fov width height scene lights ambient limit name = savePngImage name (ImageRGBF image)
	where
		image = generateImage ray width height
		ray x y = toPixel $ castRay (calcRay eye view up fov width height x y) scene lights ambient limit


-- showing off my octree to get credit for Objective 1

probeScene :: Scene -> Ray -> Maybe (Point,Int)
probeScene (Scene pos (Object t _) subs) (eye,dir)
	| isJust x = fmap (\(p,d) -> (p `add` pos,d)) x
	| null ys  = Nothing
	| otherwise = Just $ minimumBy d ys
	where
		r = (eye `sub` pos,dir)
		x = probeOctree t r
		ys = mapMaybe (flip probeScene r) subs
		d (a,_) (b,_) = compare (dist a eye) (dist b eye)

octreeRay :: Ray -> Scene -> Colour
octreeRay r@(eye,dir) s = maybe background colouring (probeScene s r)
	where
		background = (0.1,0.1,0.2)
		colouring (_,d) = mult (fromIntegral d) $ (0.1,0,0)

renderOctree :: Point  -- eye
	-> Vector          -- viewing direction
	-> Vector          -- the up direction
	-> Double          -- field of view (radians)
	-> Int             -- horizontal resolution (width)
	-> Int             -- vertical resolution (height)
	-> Scene           -- scene
	-> String          -- output file name
	-> IO ()
renderOctree eye view up fov width height scene name = savePngImage name (ImageRGBF image)
	where
		image = generateImage ray width height
		ray x y = toPixel $ octreeRay (calcRay eye view up fov width height x y) scene
