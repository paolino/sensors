{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}

module Renderers.Corr (renderer) where

import Renderer
import Graphics.Gloss
import Gloss (CColor (..))
import Library
import Control.Lens
import Control.Lens.TH
import Data.List (zipWith7)
import Data.Machine.Moore
import Data.List
import Control.Monad
import Debug.Trace
import qualified Data.Map as M

data Configuration = Configuration {
	_alpha :: Float
	, _amp :: Float
        , _radium :: Float
	, _steps :: Int
	, _persistence :: Int
	}  deriving (Show,Read)


$(makeLenses ''Configuration)

itails = init . tails

-- signal combinations
comb is = do 
        (x:xs) <- itails is
        (y:ys) <- itails xs
        (z:zs) <- itails ys
        (w:zs) <- itails zs
        return (x,y,z,w)
        
-- screen subdivision

sx = 14
sy = 9

type Point3 = (Float,Float,Float,Float)

type S = [Point3]

s0 :: S
s0 = replicate 126 (0,0,0,0)

render :: Configuration -> ((Int,Int),Point3) -> Picture
render c (tra,ps) = tracoo tra $ pointToPicture c ps

zx = 1366
zy = 768

tracoo (x,y) = 
        Translate (-zx/2) (-zy/2) . 
        Translate (zx / fromIntegral sx * (fromIntegral x)) 
                (zy / fromIntegral sy * (fromIntegral y)) . 
        Scale zx zy

pointToPicture :: Configuration -> Point3 -> Picture
pointToPicture c (x,y,z,w) = Color (makeColor d d d 1) $ Polygon [(0,0),(0,1),(1,1),(1,0)] 
        where   d' = sqrt $ (x - 0.5) ^ 2 + (y - 0.5) ^ 2 + (z-0.5)^2 + (w - 0.5)^2
                d  = if d' > (alpha `view` c) then d' else 0


picture :: Configuration -> S -> Picture
picture c = Pictures . map (render c) . zip (liftM2 (,) [0..sx-1] [0..sy-1])
{-
fade :: Configuration  -> S -> S
fade c  = map $ snd . mapAccumL f (view alpha c) where 
        f t (C (CColor r g b _) p) = (t - (view alpha c/fromIntegral (view persistence c)), C (CColor r g b t) p)
-}
conf0 = Configuration {
        _alpha = 0.3, 
        _amp = 1,
        _steps = 1, 
        _radium = 5,
        _persistence = 100 
        }
make :: Configuration -> [Float] -> S -> Renderer ()
make c qs xs = Moore ((picture c xs,())) (step  c qs xs)

step :: Configuration -> [Float] -> S -> Input -> Renderer ()


step c qs xs (Control i v) = trace (show c) $ (\op -> make (op c) qs xs) $ 
        case i of 
                48 -> set alpha v 
                47 -> set amp (v * 400) 
                46 -> set steps $ floor (v * 100)
                45 -> set persistence $  floor (v * 2000)
                44 -> set radium $ v * 10
                _ -> id
step    c qs xs
        (Sensor qs') =
                if length qs < 9 || any isNaN qs  
                        then make c qs xs
                        else make c qs' $ comb qs'

renderer = make conf0 (replicate 9 0.5) s0
