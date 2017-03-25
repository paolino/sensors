{-# LANGUAGE TemplateHaskell #-}

module Renderers.Seg1M (seg1) where

import Renderer
import Gloss
import Graphics.Gloss
import Library
import Control.Lens
import Control.Lens.TH
import Data.List (zipWith7)
import Data.Machine.Moore
import Data.List


data Configuration = Configuration {
	_alpha :: Float
	, _amp :: Float
	, _steps :: Int
	, _persistence :: Int
	, _permutation :: [Int]
	}  deriving (Show,Read)


$(makeLenses ''Configuration)

perms = permutations [0..5]

conf0 = Configuration 0.1 200 30 200 [0..5]

picture c = Pictures . map (segToPicture (view alpha c)) 

seg1 :: Renderer
seg1 = make conf0 [seg0]

make :: Configuration -> [Seg] -> Renderer
make c xs = Moore (picture c xs) (step c xs)

step :: Configuration -> [Seg] -> Input -> Renderer
step c xs (Control v@(al:am:st:pers:perm:_)) = 
	flip make [seg0] $ Configuration 
		al
		(am * 400)
		(floor $ st * 100)
		(floor $ pers * 6000)
		(perms !! (floor $ perm * 720))
step c@(Configuration _ r s p ps) xs@(Seg (CColor r0 g0 b0) (x0,y0) (x1,y1):_) (Sensor qs) =
		make c $ if length qs < 6 || any isNaN qs  then xs else take p $ reverse is ++ xs
		where
		[gx,gy,gz,ax,ay,az] = map (qs!!) ps
		linear' = linear s
		n1 = modulus2D gx ax
		n2 = modulus2D gy ay
		n3 = modulus2D gz az
		isx0 = linear' x0 (gx/n1 * r)
		isx1 = linear' x1 (gy/n2 * r)
		isy0 = linear' y0 (ax/n1 * r)
		isy1 = linear' y1 (ay/n2 * r)
		csx = linear' r0 $ gz
		csy = linear' g0 $ az
		csz = linear' b0 $ az*gz
		mkSeg r g b x0 y0 x1 y1 = Seg (CColor r g b) (x0,y0) (x1,y1)
		is = zipWith7 mkSeg csx csy csz isx1 isy1 isx0 isy0

step c xs _ = make c xs
