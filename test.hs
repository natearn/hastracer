module Main where

import Raytracer
import Octree
import Algebra
import Control.Applicative ((<$>))
import Text.ParserCombinators.Parsec (parse)
import qualified STL
import Sphere
import Texture

convert :: STL.Triangle Double -> Triangle
convert (STL.Triangle n a b c) = Triangle (f a) (f b) (f c)
	where
		f (STL.Vector x y z) = Vertex (x,y,z) []

load :: FilePath -> IO (Octree Triangle)
load file = handle =<< (parse STL.stl_file file <$> readFile file)
	where
		handle (Left msg) = error $ show msg
		handle (Right list) = return $ buildOctree (map convert list)

dump :: FilePath -> IO ([Triangle])
dump file = handle =<< (parse STL.stl_file file <$> readFile file)
	where
		handle (Left msg) = error $ show msg
		handle (Right list) = return $ map convert list

-- parameters
(xres,yres) = (256,256)
view = (0,0,-1) :: Vector
up =  (0,1,0) :: Vector
--eye = (0,0,100) :: Point
--lights = [ ((-2,-2,1),(1,1,1)),((2,2,1),(1,1,1)) ]
--lights = [ ((2,2,1),(1,1,1)) ]
fov = pi * (2/3) -- 120 degrees
amb = (0.3,0.3,0.3) :: Colour
limit = 2 :: Int
divisions = 4 :: Int

main = handle =<< (parse STL.stl_file "(stdin)" <$> getContents)
	where
		handle (Left msg) = putStrLn $ "Error: " ++ show msg
		--handle (Right list) = render eye view up fov xres yres scn lights amb limit "test.png"
		handle (Right list) = renderOctree eye view up fov xres yres scn "octree.png"
			where
				tree = buildOctree $ map convert list
				eye = (\(Box l u) -> (0,0,dist u l / 2)) $ bounds tree
				lights = [ (eye `add` (2,2,1),(1,1,1)) ]
				obj = Object tree (Phong (0.8,0.8,0.5) (0.8,0.8,0.5) 25)
				scn = Scene (-0.1,0,0) obj []
{-
-}

{-
main = render eye view up fov xres yres scn lights amb limit "test.png"
--main = renderOctree eye view up fov xres yres scn "octree.png"
	where
		tree = buildOctree $ sphere divisions
		eye = (\(Box l u) -> (0,0,dist u l / 2)) $ bounds tree
		obj = Object tree (Phong (0.9,0.5,0.5) (0.7,0.3,0.3) 25)
		obj2 = Object tree Reflection
		scn = Scene (-0.1,0,0) obj []
		--scn = Scene (1,0,0) obj [Scene (-2,0,-0.5) obj2 []]
		--scn = Scene (0,0,0) obj []
-}

{-
main = do
	img <- logo
	let tree = buildOctree $ sphere divisions
	let eye = (\(Box l u) -> (0,0,(dist u l) / 2)) $ bounds tree
	let obj = Object tree (Texture (texture tree img))
	let scn = Scene (-0.1,0,0) obj []
	render eye view up fov xres yres scn lights amb limit "test.png"
-}
