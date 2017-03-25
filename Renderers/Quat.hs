{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

module Renderers.Tri1 (renderering) where

import Graphics.UI.GLUT hiding (normalize)
import Control.Applicative
import Renderer
import Library
import Control.Lens
import Control.Lens.TH
import Data.List (zipWith7)
import Data.Machine.Moore
import Data.List
import Debug.Trace
import Linear.Quaternion hiding (rotate)
import Linear.V3
import Linear.Metric
import Data.Fixed

type Orient = Quaternion GLfloat

data Configuration = Configuration {
	_alpha :: GLfloat
	, _amp :: GLfloat
	, _steps :: Int
	, _persistence :: Int
	, _permutation :: [Int]
	}  deriving (Show,Read)


$(makeLenses ''Configuration)
conf0 = Configuration {_alpha = 0.001, _amp = 0.890625, _steps = 8, _persistence = 218, _permutation = [5,2,1,4,3,0]}


picture _ xs = do
        clear [ColorBuffer]
        mapM_ (triToPicture) $ xs

data Tri = Tri (GLfloat,GLfloat,GLfloat) Point Point Point Orient
triToPicture (Tri (ax,ay,az) p1@(x0,y0) (x1,y1) (x2,y2) q@(Quaternion o (V3 w1 w2 w3))) = preservingMatrix $ do 
	rotate ((o `mod'` 360) - 180) (Vector3 w1 w2 w3)
        renderPrimitive Triangles $ do
                color $ Color4 1 0 0 ax
                vertex (Vertex3 x0 y0 0)
                color $ Color4 0 1 0 ay
                vertex (Vertex3 x1 y1 0)
                color $ Color4 0 0 1 az
                vertex (Vertex3 x2 y2 0)

type Point = (GLfloat, GLfloat)



tri0 = Tri (1,1,1) (0,0) (0,0) (0,0)(normalize $ pure 1) 

renderering :: Renderer ()
renderering = make conf0 [tri0]




-- fade :: Configuration  -> [Tri] -> [Tri]
--fade c  = snd . mapAccumL f (view alpha c) where 
--        f t (Tri (Orient r g b _) p1 p2 p3) = (t - (view alpha c/fromIntegral (view persistence c)), Tri (CColor r g b t) p1 p2 p3)

make :: Configuration -> [Tri] -> Renderer ()
make c xs = Moore (picture c xs,()) (step c xs)

step :: Configuration -> [Tri] -> Input -> Renderer ()

step c xs  (Control i (realToFrac -> v)) = trace (show c) $  flip make xs . ($ c) $ 
        case i of 
                48 -> set alpha $ (v/100)
                47 -> set amp (v * 10)
                46 -> set steps $ floor (v * 1000)
                45 -> set persistence $  floor (v*3000)
                _ -> id

step c []  i = step c  [tri0]  i -- completeness 

step    c 
        ts@(Tri (ax0,ay0,az0) _ _ _ q : _) 
        (Sensor ys) = make c $	if length ys < 9 || any isNaN ys  then ts 
                        	else  take (view persistence c) $ ts' ++ ts
		where

                [mx,my,mz,gx,gy,gz,ax,ay,az] = map (subtract 1 . (*2) . realToFrac) ys
		
		qs' = take (view steps c) $ stripe (view alpha c) (omega gx gy gz) q
		
		ts' = reverse $ map (Tri (abs ax/100,abs ay/100,abs az/100) (-0.5,-0.5) (0.5,-0.5) (0,0.7)) qs'
		

stripe a w q = tail $ iterate (delta a w) q

omega gx gy gz = Quaternion 0 $ V3 gx gy gz
		
delta a w q = lnormalize $ q + ((a *) `fmap` (w * q))

lnormalize (Quaternion x p) = Quaternion x $ normalize p
