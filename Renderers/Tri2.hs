{-# LANGUAGE TemplateHaskell #-}

module Renderers.Tri2 (renderer) where

import Renderer
import Graphics.Gloss
import Gloss (CColor (..))
import Library
import Control.Lens
import Control.Lens.TH
import Data.List (zipWith7)
import Data.Machine.Moore
import Data.List
import Debug.Trace

data Configuration = Configuration {
	_alpha :: Float
	, _amp :: Float
--        , _qua :: Float
	, _steps :: Int
	, _persistence :: Int
	, _permutation :: [Int]
	}  deriving (Show,Read)


$(makeLenses ''Configuration)

perms = permutations [0..5]
conf0 = Configuration {
        _alpha = 0.07, 
        _amp = 203.125, 
        _steps = 12, 
        _persistence = 500, 
        _permutation = [5,2,1,4,3,0]
        }

picture _ = Pictures . map (triToPicture) . reverse 

triToPicture (Tri (CColor r g b a) p1 p2 p3) = Color (makeColor r g b a) $ Line [p1,p2,p3,p1]

data Tri = Tri CColor Point Point Point

tri0 = Tri (CColor 0 0 0 1) (0,0) (0,0) (0,0)

renderer :: Renderer ()
renderer = make conf0 [tri0]

fade :: Configuration  -> [Tri] -> [Tri]
fade c  = snd . mapAccumL f (view alpha c) where 
        f t (Tri (CColor r g b _) p1 p2 p3) = (t - (view alpha c/fromIntegral (view persistence c)), Tri (CColor r g b t) p1 p2 p3)

make :: Configuration -> [Tri] -> Renderer ()
make c xs = Moore ((picture c xs),()) (step c xs)
 
step :: Configuration -> [Tri] -> Input -> Renderer ()

step c xs (Control i v) = trace (show c) $  flip make xs . ($ c) $ 
        case i of 
                48 -> set alpha $ v/4 
                47 -> set amp (v * 400) 
                46 -> set steps $ floor (v * 100)
                45 -> set persistence $  floor (v * 2000)
                _ -> id
step c [] i = step c [tri0] i -- completeness 
step    c@(Configuration _ r s p ps) 
        xs@(Tri (CColor r0 g0 b0 _) (x0,y0) (x1,y1) (x2,y2):_) 
        (Sensor qs) =
		make c $ 
                        if length qs < 9 || any isNaN qs  
                        then xs 
                        else  fade c . take p $ reverse is ++ xs
		where
		[mx,my,mz,gx,gy,gz,ax,ay,az] = qs
		linear' = linear s
		isx0 = linear' x0 (mx * r)
		isx1 = linear' x1 (my * r)
		isx2 = linear' x2 (mz * r)
		isy0 = linear' y0 (gx * r)
		isy1 = linear' y1 (gy * r)
		isy2 = linear' y2 (gz * r)
		csx = linear' r0 ax
		csy = linear' g0 ay
		csz = linear' b0 az
		mkTri r g b x0 y0 x1 y1 x2 y2 = Tri (CColor r g b 1) (x0,y0) (x1,y1) (x2,y2)
		is = zipWith9 mkTri csx csy csz isx0 isy0 isx1 isy1 isx2 isy2

zipWith9 f a0 a1 a2 a3 a4 a5 a6 a7 a8 = zipWith ($) (zipWith8 f a0 a1 a2 a3 a4 a5 a6 a7) a8
zipWith8 f a0 a1 a2 a3 a4 a5 a6 a7  = zipWith ($) (zipWith7 f a0 a1 a2 a3 a4 a5 a6) a7
