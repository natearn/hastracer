module Raytracer where

import Data.Vect.Double
import Geometry
import Octree
import Data.List (permutations,or,minimumBy,foldl')
import Data.Maybe (mapMaybe,isJust,isNothing,fromJust,listToMaybe)
import Codec.Picture -- PNG encoding
import Codec.Picture.Types -- PNG encoding
import Data.Int

import Graphics.UI.SDL as SDL
import System.Environment
import System.Exit

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

-- better name would be nice
data Object a = Object a Material deriving Show

-- no support for rotation or scaling right now
-- ideally Scene has: transformations, object, child scenes
--data Scene a = Scene (Object a) [Scene a] deriving Show
type Scene a = [Object a]

-- Colour operations

-- https://en.wikipedia.org/wiki/Phong_lighting
phongLighting :: Vec3
	-> Vec3
	-> Material
	-> Normal3
	-> [Light]
	-> Colour
	-> Colour
phongLighting point eye (Phong kd ks shininess) normal lights ambient =
	trunc $ foldl' combine (ks &! ambient) lights -- (&!) : point-wise multiplcation
	where
	trunc (Vec3 r g b) = (Vec3 r' g' b') where [r',g',b'] = map (min 1.0) [r,g,b]
	combine :: Colour -> Light -> Colour
	combine colour (Point lpoint lcolour) -- A light has a point and a colour (RGB intensities)
		| light_vec &. normal_vec < 0 = colour
		| reflection &. eye_vec < 0 = colour &+ diffuse_val
		| otherwise  = colour &+ diffuse_val &+ specular_val
		where
		diffuse_val = ((light_vec &. normal_vec) *& (kd &! lcolour))
		specular_val = (((reflection &. eye_vec) ** shininess) *& (ks &! lcolour))
		eye_vec = normalize $ eye &- point -- direction from point to eye
		light_vec = normalize $ lpoint &- point -- direction from point to light
		reflection = reflect normal light_vec
		normal_vec = fromNormal normal

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

-- TODO: determine the best hit instead of the first hit
searchScene :: (Intersectable a)
	=> Scene a
	-> Ray
	-> Maybe (Vec3,Normal3,Material)
searchScene scn ray = listToMaybe $ mapMaybe (withMat ray) scn
	where
	withMat r (Object g m) = (\(i,n) -> Just (i,n,m)) =<< intersection g r

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

-- render the scene to a png file
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
		toPixel :: Colour -> PixelRGBF
		toPixel (Vec3 r g b) = PixelRGBF (f r) (f g) (f b)
			where f = realToFrac


-- render the scene to the screen
renderScreen :: (Intersectable a)
	=> Vec3        -- eye
	-> Normal3     -- viewing direction
	-> Normal3     -- the up direction
	-> Double      -- field of view (radians)
	-> Int         -- horizontal resolution (width)
	-> Int         -- vertical resolution (height)
	-> Scene a     -- scene
	-> [Light]     -- point lights
	-> Colour      -- ambient light
	-> Int         -- reflection limit
	-> IO ()
renderScreen eye view up fov width height scene lights ambient limit = withInit [InitVideo] $ do
		screen <- setVideoMode width height 32 [SWSurface]
		setCaption "hastracer" ""
		enableUnicode True
		surface <- createRGBSurfaceEndian [SWSurface] width height 32
		cast surface
		loop (handleEvents)
	where

		handleEvents = do
			event <- pollEvent
			case event of
				Quit -> exitWith ExitSuccess
				KeyDown (Keysym _ _ 'q') -> exitWith ExitSuccess
				NoEvent -> return ()
				_ -> handleEvents

		loop render = do
			event <- waitEvent
			case event of
				Quit -> exitWith ExitSuccess
				KeyDown (Keysym _ _ 'q') -> exitWith ExitSuccess
				_ -> return ()
			render
			loop render

		cast surface = mapM_ (castRow surface) [0..(height-1)]
		castRow surface y = mapM_ (castCell surface y) [0..(width-1)]
		castCell surface y x = do
			handleEvents
			let (Vec3 r g b) = castRay (ray x y) scene lights ambient limit
			let [r',g',b'] = map (round . (* 255.0)) [r,g,b]
			screen <- getVideoSurface
			pval <- mapRGB (surfaceGetPixelFormat screen) r' g' b'
			fillRect screen (Just (Rect x y 1 1)) pval
			SDL.flip screen

		ray x y = calcRay eye view up fov width height x y
