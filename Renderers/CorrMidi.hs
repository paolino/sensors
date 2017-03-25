{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}

module Renderers.CorrMidi (renderer, Midiv) where

import Control.Lens
import Control.Lens.TH
import Data.List (zipWith7)
import Data.Machine.Moore
import Data.List
import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Control.Arrow
import Debug.Trace
import Data.Either
import Data.Maybe
import Graphics.Gloss

import Filter (changing,  Filter)
import MealyT (runMealyT)
import Library
import Renderer
import Gloss (CColor (..))

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

type Point4 = (Float,Float,Float,Float)

value :: Point4 -> Float
value (x,y,z,w) =   sqrt $ (x - 0.5) ^ 2 + (y - 0.5) ^ 2 + (z-0.5)^2 + (w - 0.5)^2

type Midiv = (Int,Int)
type S = [Point4]

s0 :: S
s0 = replicate 126 (0,0,0,0)

render :: Configuration -> ((Int,Int),Point4) -> (Picture, Midiv)
render c (tra@(tx,ty),p) = (pic, (n, floor . (*128) $ v)) where
        v = value p
        n = tx * sy + ty
        pic = tracoo tra $ Color (makeColor v v v 1) $ Polygon [(0,0),(0,1),(1,1),(1,0)]

zx = 1366
zy = 768

tracoo (x,y) = 
        Scale zx zy .
        Translate (-1) (-1) .
        Scale (2 / fromIntegral sx) (2 / fromIntegral sy) .
        Translate (fromIntegral x) (fromIntegral y)  

picture :: Configuration -> S -> (Picture,[Midiv])
picture c xs = let
        (ps,mn) =  unzip . map (render c) . zip (liftM2 (,) [0..sx-1] [0..sy-1]) $ xs
        in (Pictures ps, mn)
{-
fade :: Configuration  -> S -> S
fade c  = map $ snd . mapAccumL f (view alpha c) where 
        f t (C (CColor r g b _) p) = (t - (view alpha c/fromIntegral (view persistence c)), C (CColor r g b t) p)
-}
conf0 = Configuration {
        _alpha = 0.1, 
        _amp = 1,
        _steps = 1, 
        _radium = 5,
        _persistence = 100 
       } 

type Eff = Filter () Midiv (Maybe Midiv)

make :: Configuration -> [Eff] -> [Float] -> S -> Renderer [Maybe Midiv]
make c ef qs xs =  let
        (p,rs) = picture c xs
        (mr,ef') = unzip . zipWith (\e r -> runReader (runMealyT e r) ()) ef $ rs
        in  Moore (p,mr) (step  c ef' qs xs)
 
step :: Configuration -> [Eff] -> [Float] -> S -> Input -> Renderer  [Maybe Midiv]

step c ef qs xs (Control i v) = trace (show c) $ (\op -> make (op c) ef qs xs) $ 
        case i of 
                48 -> set alpha v 
                47 -> set amp (v * 400) 
                46 -> set steps $ floor (v * 100)
                45 -> set persistence $  floor (v * 2000)
                44 -> set radium $ v * 10
                _ -> id
step c ef qs xs
        (Sensor qs') =
                if length qs < 9 || any isNaN qs -- a bug in serialport 
                        then make c ef qs xs
                        else make c ef qs' $ comb qs'

renderer = make conf0 (replicate 126 $ changing) (replicate 9 0.5) s0
