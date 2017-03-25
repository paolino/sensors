{-# LANGUAGE TemplateHaskell #-}

module Renderers.Seg1Un (seg1) where

import Renderer
import Gloss
import Graphics.Gloss
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
conf0 = Configuration {_alpha = 5.46875e-2, _amp = 203.125, _steps = 32, _persistence = 1046, _permutation = [5,2,1,4,3,0]}

picture _ = Pictures . map (segToPicture) . reverse

seg1 :: Renderer
seg1 = make conf0 [seg0]


fade :: Configuration  -> [Seg] -> [Seg]
fade c  = snd . mapAccumL f (view alpha c) where 
        f t (Seg (CColor r g b _) p1 p2) = (t - (view alpha c/fromIntegral (view persistence c)), Seg (CColor r g b t) p1 p2)

make :: Configuration -> [Seg] -> Renderer
make c xs = Moore ((picture c xs)) (step c xs)

step :: Configuration -> [Seg] -> Input -> Renderer

step c xs (Control v@(al:am:st:pers:perm:_)) = trace (show c) $  
	flip make [seg0] $ Configuration 
		al
		(am * 400) 
		(floor $ st * 100)
		(floor $ pers * 2000)
		(perms !! (floor $ perm * 720))

step c@(Configuration _ r s p ps) xs@(Seg (CColor r0 g0 b0 _) (x0,y0) (x1,y1):_) (Sensor qs) =
		make c $ 
                        if length qs < 9 || any isNaN qs  
                        then xs 
                        else  fade c . take p $ reverse is ++ xs
		where
		[mx,my,mz,gx,gy,gz,ax,ay,az] = qs
		linear' = linear s
		isx0 = linear' x0 (mx * r*16/9)
		isx1 = linear' x1 (my * r*16/9)
		isy0 = linear' y0 (gx * r)
		isy1 = linear' y1 (gy * r)
		csx = linear' r0 ax
		csy = linear' g0 ay
		csz = linear' b0 az
		mkSeg r g b x0 y0 x1 y1 = Seg (CColor r g b 1) (x0,y0) (x1,y1)
		is = zipWith7 mkSeg csx csy csz isx0 isy0 isx1 isy1

step c xs _ = make c xs
